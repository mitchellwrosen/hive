module Hive.Board
  ( Board
  , BoardIndex
  , initialBoard
  , boardParity
  , boardTiles
  , boardWidth
  , boardHeight
  , Parity(..)
  , Adjacency(..)
  , (!)
  , boardWinner
  , growBoard
  , occupiedNeighbors
  , occupiedNeighborIndices
  , cellsAreAdjacent
  , cellIsOccupied
  , cellIsUnoccupied
  ) where

import Mitchell.Prelude

import Data.HexBoard
import Hive.Bug
import Hive.Player
import Hive.Tile

import Data.List.NonEmpty (NonEmpty(..))
import Data.Vector        (Vector)

import qualified Data.Set    as Set
import qualified Data.Vector as Vector


type Board = HexBoard Cell


initialBoard :: Board
initialBoard = HexBoard (pure (pure [])) Even

(!) :: Board -> BoardIndex -> Cell
board ! i =
  case preview (ix i) board of
    Nothing -> error ("Hive.Board.(!): index " ++ show i ++ " out of bounds")
    Just c  -> c

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
      (True,    _) -> Just (Just P2)
      (   _, True) -> Just (Just P1)
      _            -> Nothing

-- Given an index that presumably just had a piece placed on it (or moved
-- to), possibly grow the board if that index is on an edge of the board.
growBoard :: BoardIndex -> Board -> Board
growBoard (row, col) board =
  let
    w = boardWidth board
    h = boardHeight board

    f0, f1, f2, f3 :: Board -> Board
    f0 = if row == 0   then prependRow else identity
    f1 = if row == h-1 then appendRow  else identity
    f2 = if col == 0   then prependCol else identity
    f3 = if col == w-1 then appendCol  else identity
  in
    f0 (f1 (f2 (f3 board)))

prependRow :: Board -> Board
prependRow board = over boardTilesL f board
 where
  f :: Vector (Vector [Tile]) -> Vector (Vector [Tile])
  f = Vector.cons (Vector.replicate (boardWidth board) [])

appendRow :: Board -> Board
appendRow board = over boardTilesL f board
 where
  f :: Vector (Vector [Tile]) -> Vector (Vector [Tile])
  f = flip Vector.snoc (Vector.replicate (boardWidth board) [])

prependCol :: Board -> Board
prependCol =
    over (boardTilesL . traverse) (Vector.cons [])
  . over boardParityL flipParity

appendCol :: Board -> Board
appendCol = over (boardTilesL . traverse) (flip Vector.snoc [])

-- | Get a list of neighbor cells that have at least one tile in them.
occupiedNeighbors :: Board -> BoardIndex -> [(BoardIndex, NonEmpty Tile)]
occupiedNeighbors board i =
  catMaybes
    (map (\(j,c) ->
           case c of
             []     -> Nothing
             (t:ts) -> Just (j, (t :| ts)))
         (boardNeighbors i board))

occupiedNeighborIndices :: BoardIndex -> Board -> Set BoardIndex
occupiedNeighborIndices i board =
  Set.fromList (map fst (occupiedNeighbors board i))

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
