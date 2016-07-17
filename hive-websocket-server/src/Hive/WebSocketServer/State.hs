module Hive.WebSocketServer.State
  ( ServerState(..)
  , initialServerState
  ) where

import Mitchell.Prelude

import Hive.WebSocketServer.Room

data ServerState = ServerState
  { stateRoom :: IORef Room
  -- ^ The one and only game room the server contains.
  }

initialServerState :: IO ServerState
initialServerState = do
  room <- newRoom
  roomRef <- newIORef room
  pure (ServerState roomRef)
