{-# LANGUAGE TemplateHaskell #-}

module Hive.Game where

import Hive.Board
import Hive.Bug
import Hive.Player

import Control.Lens
import Data.Aeson
import GHC.Generics (Generic)


data Game = Game
    { _gameBoard    :: !Board  -- Game board
    , _gamePlayer   :: !Player -- Current player
    , _gameP1Bugs   :: ![Bug]  -- Player 1's bugs
    , _gameP2Bugs   :: ![Bug]  -- Player 2's bugs
    , _gameP1Placed :: !Int    -- # of bugs P1 has placed
    , _gameP2Placed :: !Int    -- # of bugs P2 has placed
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
