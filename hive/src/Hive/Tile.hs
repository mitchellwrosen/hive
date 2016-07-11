module Hive.Tile
  ( Tile(..)
  , Cell
  , cellOwner
  ) where

import Mitchell.Prelude

import Hive.Bug
import Hive.Player

import Data.Aeson

-- | A tile is a Bug that belongs to a Player.
data Tile = Tile
    { tilePlayer :: Player
    , tileBug    :: Bug
    } deriving (Eq, Ord, Show)

instance ToJSON Tile where
  toJSON Tile{..} = object
    [ ("player", toJSON tilePlayer)
    , ("bug",    toJSON tileBug)
    ]

instance FromJSON Tile where
  parseJSON = withObject "object" $ \o ->
    Tile
      <$> o .: "player"
      <*> o .: "bug"

tilePlayerL :: Lens' Tile Player
tilePlayerL = lens tilePlayer (\x y -> x { tilePlayer = y })


-- A single cell is a stack of tiles, where the head of the list represents the
-- top of the stack. This will only ever be a beetle or a mosquito, per the
-- game rules.
type Cell = [Tile]

-- | Who "owns" this cell? (Meaning, whose tile is on top?)
cellOwner :: Cell -> Maybe Player
cellOwner = preview (ix 0 . tilePlayerL)
