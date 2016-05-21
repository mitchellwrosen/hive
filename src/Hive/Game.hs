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
    } deriving (Eq, Generic, Show, FromJSON, ToJSON)
makeLenses ''Game

-- instance ToJSON Game where
--   toJSON game = object
--     [ ("board", toJSON (game^.gameBoard))
--     , ("player", toJSON (game^.gamePlayer))
--     , ("p1bugs", toJSON (game^.gameP1Bugs))
--     , ("p2bugs", toJSON (game^.gameP2Bugs))
--     , ("p1placed", toJSON (game^.gameP1Placed))
--     , ("p2placed", toJSON (game^.gameP2Placed))
--     ]

-- | The state of a game: it's over, or it's active.
data GameState
    = GameOver (Maybe Player)
    | GameActive Game
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- instance
