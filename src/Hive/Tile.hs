{-# LANGUAGE TemplateHaskell #-}

module Hive.Tile where

import Mitchell.Prelude

import Hive.Bug
import Hive.Player

import Data.Aeson
import Control.Lens

-- | A tile is a Bug that belongs to a Player.
data Tile = Tile
    { _tilePlayer :: Player
    , _tileBug    :: Bug
    } deriving (Eq, Ord, Show)
makeLenses ''Tile

instance ToJSON Tile where
  toJSON Tile{..} = object
    [ ("player", toJSON _tilePlayer)
    , ("bug",    toJSON _tileBug)
    ]

instance FromJSON Tile where
  parseJSON = withObject "object" $ \o ->
    Tile
      <$> o .: "player"
      <*> o .: "bug"

-- A single cell is a stack of tiles, where the head of the list represents the
-- top of the stack. This will only ever be a beetle or a mosquito, per the
-- game rules.
type Cell = [Tile]


-- | Who "owns" this cell? (Meaning, whose tile is on top?)
cellOwner :: Cell -> Maybe Player
cellOwner xs = xs ^? ix 0 . tilePlayer
