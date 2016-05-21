module Network.WebSockets.Lifted
  ( WebSockets.PendingConnection
  , WebSockets.pendingRequest
  , rejectRequest
  , WebSockets.Connection
  , WebSockets.ConnectionOptions
  , WebSockets.defaultConnectionOptions
  ) where

import Mitchell.Prelude

import qualified Network.WebSockets as WebSockets

rejectRequest :: (MonadBase IO m) => WebSockets.PendingConnection -> ByteString -> m ()
rejectRequest conn msg = liftBase (WebSockets.rejectRequest conn msg)
