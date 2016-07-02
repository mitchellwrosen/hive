module Network.WebSockets.Lifted
  ( WebSockets.PendingConnection
  , WebSockets.pendingRequest
  , acceptRequest
  , rejectRequest
  , WebSockets.Connection
  , WebSockets.ConnectionOptions
  , WebSockets.defaultConnectionOptions
  , receiveDataMessage
  , receiveData
  , send
  , sendLog
  , sendClose
  , WebSockets.ConnectionException(..)
  , runClient
  ) where

import Mitchell.Prelude

import Control.Monad.Log

import qualified Network.WebSockets as WebSockets

acceptRequest :: MonadBase IO m => WebSockets.PendingConnection -> m WebSockets.Connection
acceptRequest = liftBase . WebSockets.acceptRequest

rejectRequest :: MonadBase IO m => WebSockets.PendingConnection -> ByteString -> m ()
rejectRequest conn msg = liftBase (WebSockets.rejectRequest conn msg)

receiveDataMessage :: MonadBase IO m => WebSockets.Connection -> m WebSockets.DataMessage
receiveDataMessage = liftBase . WebSockets.receiveDataMessage

receiveData :: MonadBase IO m => WebSockets.Connection -> m LByteString
receiveData = liftBase . WebSockets.receiveData

send :: MonadBase IO m => WebSockets.Connection -> LByteString -> m ()
send conn msg = liftBase (WebSockets.sendBinaryData conn msg)

sendLog
  :: (MonadLog msg m, MonadBase IO m)
  => WebSockets.Connection
  -> LByteString
  -> (LByteString -> msg)
  -> m ()
sendLog conn msg f = do
  logMessage (f ("[send] " ++ msg))
  send conn msg

sendClose :: MonadBase IO m => WebSockets.Connection -> m ()
sendClose conn = liftBase (WebSockets.sendClose conn ("" :: ByteString))

runClient = WebSockets.runClient
