{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hive
    ( runHive
    , module Control.Monad.Trans.Class
    , module Hive.Monad
    , module Hive.Types
    ) where

import Hive.Monad
import Hive.Types

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import qualified Data.List   as List
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Hive interpreter

runHive
    :: Monad m
    => (Game -> Hive m ())
    -> (Game -> Hive m ())
    -> m ()
runHive p1 p2 =
    runHive'
        game
        (p1 game)
        -- There's no way for the game to have a winner after just 1 turn,
        -- so this partial pattern match is ok.
        (\case
            GameActive game' -> p2 game')
  where
    game :: Game
    game = Game board P1 bugs bugs 0 0

    -- One empty tile stack
    board :: Board
    board = HexBoard (V.singleton (V.singleton [])) Even

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
    => Game                     -- Game board
    -> Hive m ()                -- Current player logic
    -> (GameState -> Hive m ()) -- Next player logic, waiting for board resulting
                                --   from current player's move
    -> m ()
runHive' game cur_player next_player =
    runFreeT cur_player >>= \case
        -- The current player's logic has ended, so we have nothing left to
        -- interpret. This shouldn't happen with well-written players.
        Pure () -> pure ()

        Free (MakePlacement bug (row, col) k)
            -- The current player must have the bug in supply.
            | bug `elem` game^.gameCurPlayerBugs

            -- Queen must be placed by move 4.
            , bug == Queen                            ||
              Queen `notElem` game^.gameCurPlayerBugs ||
              game^.gameCurPlayerPlaced /= 3

            -- The index must be in bounds, and must not have a tile on it.
            , Just [] <- game ^? gameBoard . ix (row, col)

            , let neighbors :: [Cell]
                  neighbors = boardNeighbors (row, col) (game^.gameBoard)

                  has_opponent_neighbor :: Bool
                  has_opponent_neighbor = any (\c -> cellOwner c == Just opponent) neighbors

                  has_neighbors :: Bool
                  has_neighbors = not (all null neighbors)

            -- If this is the FIRST tile, ok.
            -- If this is the SECOND TILE, it must have at least one neighbor.
            -- If this is the THIRD OR LATER tile, it must have at least one
            -- neighbor, none of which can belong to the opponent.
            , num_bugs_placed == 0                  ||
              num_bugs_placed == 1 && has_neighbors ||
              has_neighbors && not has_opponent_neighbor -> do

                let board = game^.gameBoard
                    w     = board^.boardWidth
                    h     = board^.boardHeight

                    -- Poke the new tile into place, then possibly grow the
                    -- board by 1 row/col in all 4 directions. This ensures that
                    -- valid moves can be expressed naturally in terms of row
                    -- and column indices.
                    board' :: Board
                    board' =
                          (if row == 0   then boardPrependRow else id)
                        $ (if row == h-1 then boardAppendRow  else id)
                        $ (if col == 0   then boardPrependCol else id)
                        $ (if col == w-1 then boardAppendCol  else id)
                        $ over (ix (row, col))
                               (\_ -> [Tile (game^.gamePlayer) bug])
                               board

                case boardWinner board' of
                    -- No winner yet - loop with the updated game state and
                    -- players.
                    Nothing -> do
                        let game' = game
                              & gameBoard           .~ board'
                              & gamePlayer          %~ nextPlayer
                              & gameCurPlayerBugs   %~ List.delete bug
                              & gameCurPlayerPlaced %~ (+1)

                        runHive'
                            game'
                            (next_player (GameActive game'))
                            (\result -> k (Just result))

                    -- If the game's over, run side-effects of player logic one
                    -- last time (to inform them of the game ending), but don't
                    -- bother continuing to interpret the AST. An oddly written
                    -- player may attempt to make a move after the game is said
                    -- to be over, but we don't care, we just stop interpreting.
                    Just winner -> do
                        runFreeT $ do
                            k (Just (GameOver winner))
                            next_player (GameOver winner)
                        pure ()

            | otherwise -> invalidMove k

        Free (MakeMove path k)
            -- The queen must have been played already.
            | Queen `notElem` game^.gameCurPlayerBugs

            -- A move must have at least a source and destination cell.
            , ((row,col):_:_) <- path

            -- The index of the first cell must be in bounds.
            , Just cell <- game ^? gameBoard . ix (row, col)

            -- The tile must belong to the current player
            , cellOwner cell == Just (game^.gamePlayer) ->
                error "TODO"

            | otherwise -> invalidMove k
  where
    -- Recurse with the same game state, providing Nothing to the current
    -- player's continuation to indicate an invalid move.
    invalidMove :: (Maybe GameState -> Hive m ()) -> m ()
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

    opponent :: Player
    opponent = nextPlayer (game^.gamePlayer)

    num_bugs_placed :: Int
    num_bugs_placed = game^.gameP1Placed + game^.gameP2Placed

--------------------------------------------------------------------------------
-- Misc. utils

cellOwner :: Cell -> Maybe Player
cellOwner xs = xs ^? ix 0 . tilePlayer

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- | Repeat a monadic action n times.
repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM 0 _ = pure
repeatM n f = f >=> repeatM (n-1) f

lengthLessThan2 :: [a] -> Bool
lengthLessThan2 (_:_:_) = False
lengthLessThan2 _       = True

-- | Get the winner of this board, if any.
boardWinner :: Board -> Maybe Winner
boardWinner _ = Nothing

boardPrependRow :: Board -> Board
boardPrependRow board =
    board & boardTiles %~ V.cons (V.replicate (board^.boardWidth) [])

boardAppendRow :: Board -> Board
boardAppendRow board =
    board & boardTiles %~ flip V.snoc (V.replicate (board^.boardWidth) [])

boardPrependCol :: Board -> Board
boardPrependCol board =
    board & boardTiles.traverse %~ V.cons []
          & boardParity         %~ flipParity

boardAppendCol :: Board -> Board
boardAppendCol board =
    board & boardTiles.traverse %~ flip V.snoc []
