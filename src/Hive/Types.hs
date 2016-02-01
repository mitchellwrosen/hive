{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Hive.Types
    ( module Hive.Types
    , module Data.HexBoard
    ) where

import Data.HexBoard

import Control.Lens
import GHC.Generics (Generic)

-- | Player enum.
data Player
    = P1
    | P2
    deriving (Eq, Generic, Ord, Show)

-- | Bug enum.
data Bug
    = Ant
    | Grasshopper
    | Spider
    | Beetle
    | Queen
    deriving (Eq, Generic, Ord, Show)

-- | A tile is a Bug that belongs to a Player.
data Tile = Tile
    { _tilePlayer :: Player
    , _tileBug    :: Bug
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Tile

data Winner
    = P1Wins
    | P2Wins
    | BothWin
    deriving (Eq, Generic, Show)

-- | The state of a game: it's over, or it's active.
data GameState
    = GameOver Winner
    | GameActive Game
    deriving (Eq, Generic, Show)

-- A single cell is a stack of tiles, where the head of the list represents the
-- top of the stack. This will only ever be a beetle or a mosquito, per the
-- game rules.
type Cell = [Tile]

type Board = HexBoard Cell

data Game = Game
    { _gameBoard    :: !Board  -- Game board
    , _gamePlayer   :: !Player -- Current player
    , _gameP1Bugs   :: ![Bug]  -- Player 1's bugs
    , _gameP2Bugs   :: ![Bug]  -- Player 2's bugs
    , _gameP1Placed :: !Int    -- # of bugs P1 has placed
    , _gameP2Placed :: !Int    -- # of bugs P2 has placed
    } deriving (Eq, Generic, Show)
makeLenses ''Game
