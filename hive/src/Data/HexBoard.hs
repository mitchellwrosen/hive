{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.HexBoard
    ( HexBoard(..)
    , boardTiles
    , boardParity
    , boardWidth
    , boardHeight
    , Parity(..)
    , flipParity
    ) where

import Control.Lens
import GHC.Generics (Generic)

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
    { _boardTiles  :: [[a]]
    , _boardParity :: Parity
    , _boardWidth  :: !Int
    , _boardHeight :: !Int
    } deriving (Eq, Generic, Show)
makeLenses ''HexBoard
