module Hive.Board where

import Mitchell.Prelude

import Data.HexBoard
import Hive.Bug
import Hive.Player
import Hive.Tile

import Control.Lens
import Data.Vector  (Vector)

import qualified Data.Set    as Set
import qualified Data.Vector as Vector

type Board = HexBoard Cell

-- Given an index that presumably just had a piece placed on it (or moved
-- to), possibly grow the board if that index is on an edge of the board.
growBoard :: BoardIndex -> Board -> Board
growBoard (row, col) board =
  let
    w = view boardWidth board
    h = view boardHeight board

    f0, f1, f2, f3 :: Board -> Board
    f0 = if row == 0   then prependRow else identity
    f1 = if row == h-1 then appendRow  else identity
    f2 = if col == 0   then prependCol else identity
    f3 = if col == w-1 then appendCol  else identity
  in
    f0 (f1 (f2 (f3 board)))

-- | Get the winner of this board, if any. Takes an index representing the last
-- index that a piece was either moved to or placed at as an optimization, since
-- a queen can only possibly be smothered at one of this cell's neighbors.
boardWinner :: BoardIndex -> Board -> Maybe Winner
boardWinner i0 board =
  go (catMaybes (map queenSmothered (boardNeighbors i0 board)))
 where
  queenSmothered :: (BoardIndex, Cell) -> Maybe Player
  queenSmothered (i1, cell)
    | Just (Tile player Queen) <- preview _last cell
    , length (occupiedNeighbors board i1) == 6 =
        Just player
    | otherwise = Nothing

  go :: [Player] -> Maybe Winner
  go queens =
    case (elem P1 queens, elem P2 queens) of
      (True, True) -> Just Nothing
      (True,    _) -> Just (Just P1)
      (   _, True) -> Just (Just P2)
      _            -> Nothing

prependRow :: Board -> Board
prependRow board = over boardTiles f board
 where
  f :: Vector (Vector [Tile]) -> Vector (Vector [Tile])
  f = Vector.cons (Vector.replicate (board^.boardWidth) [])

appendRow :: Board -> Board
appendRow board = over boardTiles f board
 where
  f :: Vector (Vector [Tile]) -> Vector (Vector [Tile])
  f = flip Vector.snoc (Vector.replicate (board^.boardWidth) [])

prependCol :: Board -> Board
prependCol =
    over (boardTiles . traverse) (Vector.cons [])
  . over boardParity flipParity

appendCol :: Board -> Board
appendCol = over (boardTiles . traverse) (flip Vector.snoc [])

-- | Get a list of neighbor cells that have at least one tile in them.
--
-- Postcondition: each Cell is non-empty.
occupiedNeighbors :: Board -> BoardIndex -> [(BoardIndex, Cell)]
occupiedNeighbors board i = do
  n <- boardNeighbors i board
  guard (not (null (snd n)))
  pure n

-- | A piece can slide from one cell to another if:
--
--     - The destination is unoccupied.
--     - The source and destination share precicely one occupied neighbor.
--
-- Sharing zero occupied neighbors means they are too far apart. Sharing two
-- occupied neighbors means there's too small a gap to squeeze through.
--
-- Precondition: list of indices contains at least one element.
pieceCanSlide :: BoardIndex -> [BoardIndex] -> Board -> Bool
pieceCanSlide i0 is board =
  -- No cells after the first are occupied.
  all (cellIsUnoccupied board) is &&
  -- Each two cells along the path share exactly one occupied neighbor.
  go (map (Set.fromList . occupiedNeighbors board) (i0:is))
 where
  go :: [Set (BoardIndex, Cell)] -> Bool
  go [] = True -- should never be reached
  go [_] = True
  go (s0:s1:ss) = length (Set.intersection s0 s1) == 1 && go (s1:ss)

-- | Two cells are adjacent if one of them is a neighbor of the other.
cellsAreAdjacent :: Board -> BoardIndex -> BoardIndex -> Bool
cellsAreAdjacent board src dst =
  dst `elem` map fst (boardNeighbors src board)

-- | Is this cell occupied by at least one tile?
cellIsOccupied :: Board -> BoardIndex -> Bool
cellIsOccupied board i = isJust (preview (ix i . _head) board)

-- | Is this cell unoccupied (but still in bounds)?
cellIsUnoccupied :: Board -> BoardIndex -> Bool
cellIsUnoccupied board i = preview (ix i) board == Just []
