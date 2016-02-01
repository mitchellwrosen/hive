{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Data.HexBoard
    ( HexBoard(..)
    , boardTiles
    , boardParity
    , boardWidth
    , boardHeight
    , boardNeighbors
    , Parity(..)
    , flipParity
    ) where

import Control.Lens
import Data.Maybe
import Data.Vector  (Vector)
import GHC.Generics (Generic)

import qualified Data.Vector as V

-- Even or odd board parity.
data Parity
    = Even
    | Odd
    deriving (Eq, Generic, Show)

flipParity :: Parity -> Parity
flipParity Even = Odd
flipParity Odd  = Even

-- | Hexagonal board.
--
-- Uses the "odd-q" or "even-q" vertical layout (tracked by boardParity field),
-- as depected here:
-- http://www.redblobgames.com/grids/hexagons/#coordinates
--
-- Invariant: each row contains the same number of columns as the others (the
--            board is a rectangle)

-- Invariant: boardWidth and boardHeight are always kept in sync with boardTiles
data HexBoard a = HexBoard
    { _boardTiles  :: Vector (Vector a)
    , _boardParity :: Parity
    } deriving (Eq, Generic, Show)
makeLenses ''HexBoard

type instance Index   (HexBoard a) = (Int, Int)
type instance IxValue (HexBoard a) = a

instance Ixed (HexBoard a) where
    ix (row, col) = boardTiles . ix row . ix col

boardWidth :: Getter (HexBoard a) Int
boardWidth = to (\board -> maybe 0 length (board ^? boardTiles . ix 0))

boardHeight :: Getter (HexBoard a) Int
boardHeight = boardTiles . to length

boardNeighbors :: (Int, Int) -> HexBoard a -> [a]
boardNeighbors (row, col) board = catMaybes (map (\i -> board ^? ix i) indices)
  where
    indices :: [(Int, Int)]
    indices =
        case tileParity of
            Even -> [up, right, downright, down, downleft, left]
            Odd  -> [up, upright, right, down, left, upleft]

    up        = (row-1, col)
    upright   = (row-1, col+1)
    right     = (row,   col+1)
    downright = (row+1, col+1)
    down      = (row+1, col)
    downleft  = (row+1, col-1)
    left      = (row,   col-1)
    upleft    = (row-1, col-1)

    colParity :: Parity
    colParity
        | even col  = Even
        | otherwise = Odd

    tileParity :: Parity
    tileParity
        | board^.boardParity == colParity = Even
        | otherwise = Odd
