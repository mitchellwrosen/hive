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
import Data.List.NonEmpty        (NonEmpty(..))
import Data.Maybe
import Data.Set                  (Set)
import Data.Vector               (Vector)

import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S
import qualified Data.Vector        as V

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
runHive' game cur_player next_player = do
    let board = game^.gameBoard

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
            , Just [] <- board ^? ix (row, col)

            , let neighbors :: [Cell]
                  neighbors = map snd (boardNeighbors (row, col) board)

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

                let w = board^.boardWidth
                    h = board^.boardHeight

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

        Free (MakeMove src path k)
            -- The queen must have been played already.
            | Queen `notElem` game^.gameCurPlayerBugs

            -- The index of the first cell must be in bounds.
            , Just cell <- board ^? ix src

            -- There must be a tile there to move.
            , (Tile p bug : _) <- cell

            -- The tile must belong to the current player.
            , p == game^.gamePlayer

            -- The move must be valid per the movement rules of the game.
            , isValidMove bug src path board ->
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

-- | Is this a valid move?
--
-- Checks the following conditions:
--
--     - This move doesn't violate the "One Hive Rule" (the tiles must always be
--       part of the same strongly connected component, even during movement).
--
--     - This move doesn't violate the "Freedom to Move Rule" (a piece must be
--       able to slide freely along its path).
--
--     - The per-bug movement rules are adhered to.
--
-- However, this function assumes that the given bug is indeed allowed to move
-- per other game rules, such as whose turn it is, whether or not the queen has
-- been placed yet, and whether or not any beetles are prohibiting movement.
--
-- Precondition: the source tile has at least one bug on it (the bug being
-- moved).
isValidMove :: Bug -> BoardIndex -> NonEmpty BoardIndex -> Board -> Bool

-- "One Hive Rule".
--
-- Note that this only checks the number of strongly connected components that
-- result from removing the piece from the board, so a piece removed from the
-- edge of the board will pass this check, even though it might become its own
-- island if moved to its destination.
--
-- So, this implements more like 75% of the "One Hive Rule".
--
-- ('tail' is safe here per the precondition.)
isValidMove _ src _ board | boardSCCs (over (ix src) tail board) /= 1 = False

-- An ant slides around.
isValidMove Ant i0 (i1 :| is) board =
    pieceCanSlide i0 (i1:is) board

-- A spider slides one cell at a time, and cannot backtrack.
isValidMove Spider i0 (i1 :| [i2,i3]) board =
    pieceCanSlide i0 [i1,i2,i3] board &&
    List.nub [i0,i1,i2,i3] == [i0,i1,i2,i3]

-- A beetle moves to an adjacent tile with at least one neighbor that wasn't
-- its original position. This prevents a beetle from popping off the edge of
-- the hive and becoming its own island.
isValidMove Beetle i0 (i1 :| []) board =
    let
        neighbors :: [BoardIndex]
        neighbors = map fst (boardNeighbors i1 board)
    in
        cellsAreAdjacent i0 i1 board &&
        neighbors /= [] &&
        neighbors /= [i0]

-- A queen slides one cell.
isValidMove Queen i0 (i1 :| []) board =
    pieceCanSlide i0 [i1] board

-- | A piece can slide from one cell to another if:
--
--     - The destination is unoccupied.
--     - The source and destination share precicely one neighbor.
--
-- Zero neighbors means they are too far apart. Two neighbors means
-- there's too small a gap to squeeze through.
--
-- Precondition: list of indices contains at least one element.
pieceCanSlide :: BoardIndex -> [BoardIndex] -> Board -> Bool
pieceCanSlide i0 is board =
    -- No cells after the first are occupied (tail is safe per the precondition)
    and (map (\i -> board ^? ix i == Just []) is) &&
    -- Each two cells along the path share exactly one neighbor.
    go (map (\i -> S.fromList (boardNeighbors i board)) (i0:is))
  where
    go :: [Set (BoardIndex, Cell)] -> Bool
    go [] = True -- should never be reached
    go [_] = True
    go (s0:s1:ss) = length (S.intersection s0 s1) == 1 && go (s1:ss)

-- | Two cells are adjacent if one of them is a neighbor of the other.
cellsAreAdjacent :: BoardIndex -> BoardIndex -> Board -> Bool
cellsAreAdjacent src dst board =
    dst `elem` map fst (boardNeighbors src board)

boardSCCs :: HexBoard a -> Int
boardSCCs = error "TODO"

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
