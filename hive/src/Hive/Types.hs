{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Hive.Types where

import Data.HexBoard        (HexBoard)
import Data.HexBoard.Zipper (HexBoardZ)

import Control.Lens

-- | Player enum.
data Player
    = P1
    | P2
    deriving (Eq, Show)

-- | Bug enum.
data Bug
    = Ant
    | Grasshopper
    | Spider
    | Beetle
    | Queen
    deriving (Eq, Show)

-- | A tile is a Bug that belongs to a Player.
data Tile = Tile
    { _tilePlayer :: Player
    , _tileBug    :: Bug
    } deriving (Eq, Show)
makeLenses ''Tile

data Winner
    = P1Wins
    | P2Wins
    | BothWin

-- | The state of a game: it's over, or it's active.
data GameState
    = GameOver Winner
    | GameActive Game

-- (row, col)
--
-- For example, on a 5-by-5 board,
--
--     (0,0) represents the top left tile
--     (0,4) represents the top right tile
type BoardIndex = (Int, Int)

-- A single cell is a stack of tiles, where the head of the list represents the
-- top of the stack. This will only ever be a beetle or a mosquito, per the
-- game rules.
type Cell = [Tile]

type Board = HexBoard Cell

type BoardZ = HexBoardZ Cell

data Game = Game
    { _gameBoard    :: !Board  -- Game board
    , _gamePlayer   :: !Player -- Current player
    , _gameP1Bugs   :: ![Bug]  -- Player 1's bugs
    , _gameP2Bugs   :: ![Bug]  -- Player 2's bugs
    , _gameP1Placed :: !Int    -- # of bugs P1 has placed
    , _gameP2Placed :: !Int    -- # of bugs P2 has placed
    }
makeLenses ''Game
