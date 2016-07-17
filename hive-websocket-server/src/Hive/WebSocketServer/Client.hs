module Hive.WebSocketServer.Client
  ( Client(..)
  , newClient
  , clientRecvLoop
  , clientSendLoop
  ) where

import Mitchell.Prelude

import Hive.WebSocketServer.Message

import Data.Aeson

import qualified Network.WebSockets as WebSockets

data Client = Client
  { clientUuid     :: Text
  , clientConn     :: WebSockets.Connection
  , clientRecvChan :: TChan InMessage
  -- ^ Channel of messages received by the client.
  , clientSendChan :: TChan OutMessage
  -- ^ Channel of messages to be sent to the client.
  }

newClient :: Text -> WebSockets.Connection -> IO Client
newClient uuid conn =
  Client uuid conn
    <$> newTChanIO
    <*> newTChanIO

-- | Receive messages on the client connection and send them to the client
-- receive channel.
clientRecvLoop :: Client -> IO ()
clientRecvLoop Client{..} = forever $ do
  bytes <- WebSockets.receiveData clientConn
  atomicallySTM . writeTChan clientRecvChan $
    case decodeStrict' bytes of
      Nothing -> InMessageUnknown bytes
      Just action -> InMessageAction action

-- | Receive messages on the client send channel and send them to the client
-- connection.
clientSendLoop :: Client -> IO ()
clientSendLoop Client{..} = forever $ do
  msg <- atomicallySTM (readTChan clientSendChan)
  WebSockets.sendTextData clientConn (encode msg)
