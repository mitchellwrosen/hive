module Hive.WebSocketServer.Room where

import Mitchell.Prelude

import Hive.WebSocketServer.Client

import Hive

data Room = Room
  { roomPlayer1 :: TMVar (Maybe Client)
  , roomPlayer2 :: TMVar (Maybe Client)
  , roomGame    :: TVar GameState
  }

newRoom :: IO Room
newRoom = Room
  <$> newEmptyTMVarIO
  <*> newEmptyTMVarIO
  <*> newTVarIO (initialGameState True True)
