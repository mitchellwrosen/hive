{-# LANGUAGE TemplateHaskell #-}

module Hive.Game where

import Mitchell.Prelude

import Hive.Board
import Hive.Bug
import Hive.Player

import Control.Lens
import Data.Aeson


data Game = Game
    { _gameBoard   :: !Board  -- Game board
    , _gamePlayer  :: !Player -- Current player
    , _gameBugs    :: ![Bug]  -- Current player's bugs
    , _gameBugs'   :: ![Bug]  -- Next player's bugs
    , _gamePlaced  :: !Int    -- # of bugs current player has placed
    , _gamePlaced' :: !Int    -- # of bugs next player has placed
    } deriving (Eq, Show)
makeLenses ''Game

instance ToJSON Game where
  toJSON game = object
    [ ("board",   toJSON (view gameBoard   game))
    , ("player",  toJSON (view gamePlayer  game))
    , ("bugs",    toJSON (view gameBugs    game))
    , ("bugs'",   toJSON (view gameBugs'   game))
    , ("placed",  toJSON (view gamePlaced  game))
    , ("placed'", toJSON (view gamePlaced' game))
    ]

instance FromJSON Game where
  parseJSON = withObject "object" $ \o ->
    Game
      <$> o .: "board"
      <*> o .: "player"
      <*> o .: "bugs"
      <*> o .: "bugs'"
      <*> o .: "placed"
      <*> o .: "placed'"

-- | The state of a game: it's over, or it's active.
data GameState
    = GameOver (Maybe Player)
    | GameActive Game
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
