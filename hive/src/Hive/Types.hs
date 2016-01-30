{-# LANGUAGE TemplateHaskell #-}

module Hive.Types where

import Control.Lens

-- | Player 1 or Player 2
data Player
    = P1
    | P2
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

-- | Direction from a tile.
data Adjacency
    = UpLeft
    | Up
    | UpRight
    | DownRight
    | Down
    | DownLeft
    | Center -- Directly on top, like a beetle

-- | A path through the game board that a piece takes.
--
-- Invariant: this list always contains at least two elements.
type Path = [(Int, Int)]

-- | Hive game board, represented as a 2D array of vertical
-- stacks of hexagonal tiles.
--
-- Uses the "odd-q" vertical layout, as depected here:
-- http://www.redblobgames.com/grids/hexagons/#coordinates
type Board = [[TileStack]]

-- Traversal over all the tiles on the board.
boardTiles :: Traversal' [[TileStack]] Tile
boardTiles = traverse . traverse . traverse

-- | A move from one tile to another.
data Move = Move Path Adjacency

-- | The placing of a new tile.
data Placement = Placement Tile (Int, Int) Adjacency

-- | The winner of a game.
data Winner
    = P1Wins
    | P2Wins
    | BothWin

-- | The result of taking a single turn: game over (possibly after
-- opponent takes turn), or an updated game (after opponent takes turn).
data TurnResult
    = GameOver Winner
    | GameCont Game

data Game = Game
    { _gameBoard  :: Board  -- Game board
    , _gamePlayer :: Player -- Current player
    , _gameP1Bugs :: [Bug]  -- Player 1's unplayed bugs
    , _gameP2Bugs :: [Bug]  -- Player 2's unplayed bugs
    }
makeLenses ''Game
