{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Mitchell.Prelude -- TODO: lifted-base

import Hive
import Hive.Impl.Common

import qualified Network.Wai.Lifted                    as Wai
import qualified Network.Wai.Handler.Warp.Lifted       as Wai.Warp
import qualified Network.Wai.Handler.WebSockets.Lifted as Wai.WebSockets
import qualified Network.WebSockets.Lifted             as WebSockets

import Control.Monad.Log
import Control.Monad.Trans.State.Ref.Extra
import Control.Monad.Trans.Unlift
import Data.Atomics

import qualified Data.Aeson         as Aeson
import qualified Network.HTTP.Types as HTTP

type RoomMap = Map ByteString Room


data Room = Room
  { player1 :: MVar (Maybe ())
  , player2 :: MVar (Maybe ())
  }

main :: IO ()
main = do
  p1 <- newEmptyMVar
  p2 <- newEmptyMVar
  runStateIORefT (runLoggingT main' putStrLn) (Room p1 p2)
  pure ()

main' :: LoggingT Text (StateRefT IORef Room IO) ()
main' = do
  logMessage ("Starting server on port " ++ show port)
  Wai.Warp.run port (Wai.WebSockets.websocketsOr opts wsApp httpApp)
 where
  port :: Wai.Warp.Port
  port = 6789

  opts :: WebSockets.ConnectionOptions
  opts = WebSockets.defaultConnectionOptions

wsApp
  :: WebSockets.PendingConnection
  -> LoggingT Text (StateRefT IORef Room IO) ()
wsApp pending_conn = do
  logMessage ("Pending connection: " ++ show (WebSockets.pendingRequest pending_conn))

  Room p1 p2 <- getState

  let go [] = do
        logMessage "Player rejected from room"
        WebSockets.rejectRequest pending_conn "Room is full"
      go (m:ms) =
        liftBase (tryPutMVar m Nothing) >>= \case
          True -> do
            logMessage "Player joined room"
            liftBase (swapMVar p1 (Just ()))
            WebSockets.rejectRequest pending_conn "You are playing"
          False -> go ms

  go [p1,p2]

  -- ticket <- getStateCAS

  -- room_ticket <- liftIO . readForCAS =<< getState


  -- logMessage "Waiting for player 1..."

  -- p1 <- WebSockets.acceptRequest pending_conn

  -- (s1, _) <- Socket.accept pendin
  -- undefined

  -- sock <- WebSockets.makeListenSocket "127.0.0.1" 6789 >>= serve

-- server :: WebSockets.PendingConnection -> IO ()
-- server =

-- serve :: WebSockets.Connection -> IO ()
-- serve sock = do
--   putStrLn "waiting for player 1..."
--   (s1, _) <- accept sock
--   p1 <- WebSockets.acceptRequest
--           =<< WebSockets.makePendingConnection s1 WebSockets.defaultConnectionOptions
--
--   putStrLn "waiting for player 2..."
--   (s2, _) <- accept sock
--   p2 <- WebSockets.acceptRequest
--           =<< WebSockets.makePendingConnection s2 WebSockets.defaultConnectionOptions
--
--   runHive (wsShim p1) (wsShim p2)
--
-- -- | Receive encoded actions over a WebSocket connection and embed them in the
-- -- Hive monad.
-- wsShim :: WebSockets.Connection -> Game -> Hive IO ()
-- wsShim conn game = do
--   liftIO (WebSockets.sendBinaryData conn (Aeson.encode game))
--   loop
--  where
--   loop = do
--     bytes <- liftIO (WebSockets.receiveData conn)
--     case Aeson.decode bytes of
--       Nothing -> putStrLn "Error decoding action"
--       Just action -> do
--         result <-
--           case action of
--             Place bug idx -> makePlacement bug idx
--             Move i0 is -> makeMove i0 is
--
--         liftIO (WebSockets.sendBinaryData conn (Aeson.encode result))
--
--         case result of
--           Right (GameOver _) -> liftIO (gracefulClose conn)
--           _ -> loop
--
-- gracefulClose :: WebSockets.Connection -> IO ()
-- gracefulClose conn = do
--   WebSockets.sendClose conn ("" :: ByteString)
--   -- Wait up to 5 seconds for the other party to send a close message.
--   _ <- race waitForClose (threadDelay 5000000)
--   pure ()
--  where
--   waitForClose =
--     catchJust
--       (\case
--         WebSockets.ConnectionClosed -> Just ()
--         _ -> Nothing)
--       (forever (void (WebSockets.receiveDataMessage conn)))
--       (\_ -> pure ())

httpApp :: Wai.Application m
httpApp req resp = do
  resp (Wai.responseLBS HTTP.status400 [] "Hello, stranger")
