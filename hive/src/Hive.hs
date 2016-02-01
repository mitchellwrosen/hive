{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hive
    ( runHive
    , makeMove
    , makePlacement
    , module Hive.Types
    , module Control.Monad.Trans.Class
    ) where

import Data.HexBoard
import Data.HexBoard.Zipper (HexBoardZ)
import Hive.Monad
import Hive.Types

import qualified Data.HexBoard.Zipper as BZ

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class

import qualified Data.List as List

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
    board = HexBoard [[[]]] Even 1 1

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
            -- Placing a bug that's not in the current player's supply is an
            -- invalid placement.
            | bug `notElem` game^.gameCurPlayerBugs -> invalidMove k

            -- Queen must be placed by move 4.
            | bug /= Queen &&
              Queen `elem` game^.gameCurPlayerBugs &&
              game^.gameCurPlayerPlaced == 3 -> invalidMove k

            | otherwise ->
                case boardAtIndex (row, col) (game^.gameBoard) of
                    Just z | null (BZ.peek z) -> do
                        let neighbors :: [Cell]
                            neighbors = BZ.neighbors z

                            -- Does the focused tile have any enemy neighbors?
                            has_opponent_neighbor :: Bool
                            has_opponent_neighbor = any p neighbors
                              where
                                p :: Cell -> Bool
                                p cell = cellOwner cell == Just opponent

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
                                let w = game^.gameBoard.boardWidth
                                    h = game^.gameBoard.boardHeight

                                    board' :: Board
                                    board' =
                                          BZ.toBoard
                                        $ BZ.poke [Tile (game^.gamePlayer) bug]
                                        $ (if row == 0   then BZ.insertRowAbove [] else id)
                                        $ (if row == h-1 then BZ.insertRowBelow [] else id)
                                        $ (if col == 0   then BZ.insertColLeft  [] else id)
                                        $ (if col == w-1 then BZ.insertColRight [] else id)
                                        $ z

                                case boardWinner board' of
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

--------------------------------------------------------------------------------
-- Misc. utils

-- | Zip to the specified index on a board.
boardAtIndex :: BoardIndex -> Board -> Maybe BoardZ
boardAtIndex (row, col) =
    BZ.fromBoard >=>
    repeatM row BZ.down >=>
    repeatM col BZ.right

-- | Get the winner of this board, if any.
boardWinner :: Board -> Maybe Winner
boardWinner _ = Nothing

cellOwner :: Cell -> Maybe Player
cellOwner xs = xs ^? ix 0 . tilePlayer

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- | Repeat a monadic action n times.
repeatM :: Monad m => Int -> (a -> m a) -> a -> m a
repeatM 0 _ = pure
repeatM n f = f >=> repeatM (n-1) f
