module Main where

import Mitchell.Prelude

import Hive
import Hive.Impl.Common

import Network.Socket

import qualified Data.Aeson         as Aeson
import qualified Network.WebSockets as WebSockets

-- Quick-and-dirty main that never dies, but only plays out one game.
main :: IO ()
main = do
  sock <- WebSockets.makeListenSocket "127.0.0.1" 6789

  putStrLn "waiting for player 1..."
  (s1, _) <- accept sock
  p1 <- WebSockets.acceptRequest
          =<< WebSockets.makePendingConnection s1 WebSockets.defaultConnectionOptions

  putStrLn "waiting for player 2..."
  (s2, _) <- accept sock
  p2 <- WebSockets.acceptRequest
          =<< WebSockets.makePendingConnection s2 WebSockets.defaultConnectionOptions

  runHive (wsShim p1) (wsShim p2)

-- | Receive encoded actions over a WebSocket connection and embed them in the
-- Hive monad.
wsShim :: WebSockets.Connection -> Game -> Hive IO ()
wsShim conn game = do
  liftIO (WebSockets.sendBinaryData conn (Aeson.encode game))
  loop
 where
  loop = do
    bytes <- liftIO (WebSockets.receiveData conn)
    case Aeson.decode bytes of
      Nothing -> putStrLn "Error decoding action"
      Just action -> do
        result <-
          case action of
            Place bug idx -> makePlacement bug idx
            Move i0 is -> makeMove i0 is

        liftIO (WebSockets.sendBinaryData conn (Aeson.encode result))

        case result of
          Right (GameOver _) -> liftIO (gracefulClose conn)
          _ -> loop

gracefulClose :: WebSockets.Connection -> IO ()
gracefulClose conn = do
  WebSockets.sendClose conn ("" :: ByteString)
  -- Wait up to 5 seconds for the other party to send a close message.
  _ <- race waitForClose (threadDelay 5000000)
  pure ()
 where
  waitForClose =
    catchJust
      (\case
        WebSockets.ConnectionClosed -> Just ()
        _ -> Nothing)
      (forever (void (WebSockets.receiveDataMessage conn)))
      (\_ -> pure ())
