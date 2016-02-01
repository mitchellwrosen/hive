{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Hive.Types where

import Control.Lens

-- | Player 1 or Player 2
data Player
    = P1
    | P2
    deriving (Eq, Show)

data Color
    = White
    | Black
    deriving (Eq, Show)

-- | Bug type.
data Bug
    = Ant
    | Grasshopper
    | Spider
    | Beetle
    | Queen
    deriving (Eq, Show)

-- | A tile is a Bug and belongs to a Player.
data Tile
    = Tile Player Bug
    deriving (Eq, Show)

-- A stack of tiles, with the topmost tile at the head of the list.
type TileStack = [Tile]

-- (row, col)
--
-- For example, on a 5-by-5 board,
--
--     (0,0) represents the top left tile
--     (0,4) represents the top right tile
type BoardIndex = (Int, Int)

-- | A path through the game board that a piece takes.
--
-- Invariant: this list always contains at least two elements.
type Path = [BoardIndex]

-- Even or odd board parity.
-- http://www.redblobgames.com/grids/hexagons/#coordinates
data Parity
    = Even
    | Odd
    deriving (Eq, Show)

-- | Hive game board, represented as a 2D array of vertical stacks of hexagonal
-- tiles. When a player places a piece on the edge of the board, it resizes to
-- accomodate one additional row or column. Thus, the board always has enough
-- "room" for any possible move.
--
-- Uses the "odd-q" or "even-q" vertical layout (tracked by boardParity field),
-- as depected here:
-- http://www.redblobgames.com/grids/hexagons/#coordinates
--
-- Invariant: each row contains the same number of columns as the others (the
--            board is a rectangle)
-- Invariant: each row contains one or more columns (so, perhaps
--            a more appropriate type is [NonEmpty TileStack])
-- Invariant: there is always at least one row (the empty board is 1x1, not 0x0)
data Board = Board
    { _boardTiles  :: [[TileStack]]
    , _boardParity :: Parity
    , _boardWidth  :: !Int
    , _boardHeight :: !Int
    } deriving Show
makeLenses ''Board

-- | The winner of a game.
data Winner
    = P1Wins
    | P2Wins
    | BothWin
    deriving Show

-- | The result of taking a single turn: game over (possibly after
-- opponent takes turn), or an updated game (after opponent takes turn).
data TurnResult
    = GameOver Winner
    | GameCont Board [Bug] [Bug]

data Game = Game
    { _gameBoard    :: !Board  -- Game board
    , _gamePlayer   :: !Player -- Current player
    , _gameP1Bugs   :: ![Bug]  -- Player 1's bugs
    , _gameP2Bugs   :: ![Bug]  -- Player 2's bugs
    , _gameP1Placed :: !Int    -- # of bugs P1 has placed
    , _gameP2Placed :: !Int    -- # of bugs P2 has placed
    }
makeLenses ''Game
