{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hive
    ( Hive
    , makePlacement
    , makeMove
    , runHive
    , module Hive.Types
    , module Control.Monad.Trans.Class
    ) where

import Hive.BoardZipper
import Hive.Types

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import qualified Data.List as List

--------------------------------------------------------------------------------
-- Hive monad

-- | Underlying Hive action functor.
data HiveF a
    -- Place a new tile.
    = MakePlacement
        Bug                      -- The bug to place
        BoardIndex               -- The cell to place it at
        (Maybe TurnResult -> a)  -- The resulting board state (after opp. goes)
    -- Move an already-placed tile.
    | MakeMove
        [BoardIndex]             -- The path from source cell to dest. cell
        (Maybe TurnResult -> a)  -- The resulting board state (after opp. goes)
    deriving Functor

-- The Hive monad, where Hive actions and inner monad actions are interleaved.
type Hive m a = FreeT HiveF m a

--------------------------------------------------------------------------------
-- Hive DSL

makePlacement
    :: Monad m
    => Bug
    -> BoardIndex
    -> Hive m (Maybe TurnResult)
makePlacement bug idx = liftF (MakePlacement bug idx id)

makeMove :: Monad m => [BoardIndex] -> Hive m (Maybe TurnResult)
makeMove path = liftF (MakeMove path id)

--------------------------------------------------------------------------------
-- Hive interpreter

runHive
    :: Monad m
    => (Board -> [Bug] -> [Bug] -> Hive m ())
    -> (Board -> [Bug] -> [Bug] -> Hive m ())
    -> m ()
runHive p1 p2 =
    runHive'
        game
        (p1 board bugs bugs)
        -- There's no way for the game to have a winner after just 1 turn,
        -- so this partial pattern match is ok.
        (\case
            GameCont board' _ bugs' ->
                p2 board' -- board with 1 bug on it
                   bugs   -- p2 hasn't gone yet, so just re-use initial bugs
                   bugs') -- p1 has played one bug
  where
    game :: Game
    game = Game board P1 bugs bugs 0 0

    -- One empty tile stack
    board :: Board
    board = Board [[[]]]

    bugs :: [Bug]
    bugs =
        [ Ant, Ant, Ant
        , Grasshopper, Grasshopper, Grasshopper
        , Spider, Spider
        , Beetle, Beetle
        , Queen
        ]

-- Invariant: the first Hive monad represents the current player
-- (this lets us avoid a case-analysis of game^.gamePlayer)
runHive'
    :: forall m.
       Monad m
    => Game                                    -- Game board
    -> Hive m ()                               -- Current player logic
    -> (TurnResult -> Hive m ())               -- Next player logic, waiting for board resulting
                                               --   from current player's move
    -> m ()
runHive' game cur_player next_player =
    runFreeT cur_player >>= \case
        -- The current player's logic has ended, so we have nothing left to
        -- interpret. This shouldn't happen with well-written players.
        Pure () -> pure ()

        Free (MakePlacement bug (x,y) k)
            -- Placing a bug that's not in the current player's supply is an
            -- invalid placement.
            | bug `notElem` game^.gameCurPlayerBugs -> invalidMove k

            -- Queen must be placed by move 4.
            | bug /= Queen &&
              Queen `elem` game^.gameCurPlayerBugs &&
              game^.gameCurPlayerPlaced == 3 -> invalidMove k

            | otherwise ->
                case boardAtIndex (x,y) (game^.gameBoard) of
                    Just z | bzPeek z == [] -> do
                        let neighbors :: [TileStack]
                            neighbors = bzNeighbors z

                            -- Does the focused tile have any enemy neighbors?
                            has_opponent_neighbor :: Bool
                            has_opponent_neighbor = any p neighbors
                              where
                                p :: TileStack -> Bool
                                p tile = tileOwner tile == Just opponent

                                opponent :: Player
                                opponent = nextPlayer (game^.gamePlayer)

                            -- Does the focused tile have no neighbors whatsoever?
                            has_no_neighbors :: Bool
                            has_no_neighbors = all null neighbors

                            num_placed :: Int
                            num_placed = game^.gameP1Placed + game^.gameP2Placed

                        if -- Turn two or later: tile must not have any opponent
                           -- neighbors, and must also have at least one
                           -- neighbor
                           (num_placed > 1 && (has_opponent_neighbor || has_no_neighbors)) ||
                           -- Turn one: tile must have at least one neighbor
                           (num_placed == 1 && has_no_neighbors)
                           -- Turn zero: neither of these two conditions apply
                            then invalidMove k
                            else do
                                let -- TODO: Possibly grow board
                                    z'     = bzPoke [Tile (game^.gamePlayer) bug] z
                                    board' = bzToBoard z'

                                case boardWinner board' of
                                    Nothing -> do
                                        let game' = game
                                              & gameBoard           .~ board'
                                              & gamePlayer          %~ nextPlayer
                                              & gameCurPlayerBugs   %~ List.delete bug
                                              & gameCurPlayerPlaced %~ (+1)

                                        runHive'
                                            game'
                                            (next_player (gameToResult game'))
                                            (\result -> k (Just result))

                                    Just winner -> do
                                        -- Run side-effects of player logic
                                        -- resulting from a game ending, but
                                        -- don't bother continuing to interpret
                                        -- the player actions.
                                        runFreeT $ do
                                            k (Just (GameOver winner))
                                            next_player (GameOver winner)
                                        pure ()

                    -- This index is out of bounds, or there's one or more tiles
                    -- here. Either way, it's an invalid placement.
                    _ -> invalidMove k

        Free (MakeMove path k) ->
            error "TODO"
  where
    -- Recurse with the same game state, providing Nothing to the current
    -- player's continuation to indicate an invalid move.
    invalidMove :: (Maybe TurnResult -> Hive m ()) -> m ()
    invalidMove k = runHive' game (k Nothing) next_player

    -- Lens onto the current player's unplaced bugs.
    gameCurPlayerBugs :: Lens' Game [Bug]
    gameCurPlayerBugs =
        case game^.gamePlayer of
            P1 -> gameP1Bugs
            P2 -> gameP2Bugs

    -- Lens onto the current player's # of placed bugs.
    gameCurPlayerPlaced :: Lens' Game Int
    gameCurPlayerPlaced =
        case game^.gamePlayer of
            P1 -> gameP1Placed
            P2 -> gameP2Placed

-- | Zip to the specified index on a board.
boardAtIndex :: BoardIndex -> Board -> Maybe BoardZipper
boardAtIndex (x,y) =
    bzFromBoard       >=>
    repeatM x bzRight >=>
    repeatM y bzDown

-- | Get the winner of this board, if any.
boardWinner :: Board -> Maybe Winner
boardWinner _ = Nothing

tileOwner :: TileStack -> Maybe Player
tileOwner [] = Nothing
tileOwner (Tile player _ : _) = Just player

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- | Repeat a monadic action n times.
repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM 0 _ = pure
repeatM n f = f >=> repeatM (n-1) f

-- | Strip a Game to just the elements that are passed on to players in a
-- TurnResult.
gameToResult :: Game -> TurnResult
gameToResult Game{..} = GameCont _gameBoard bugs1 bugs2
  where
    bugs1 = if _gamePlayer == P1 then _gameP1Bugs else _gameP2Bugs
    bugs2 = if _gamePlayer == P2 then _gameP1Bugs else _gameP2Bugs
