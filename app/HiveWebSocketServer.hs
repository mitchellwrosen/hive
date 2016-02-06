{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hive
import Hive.Impl.Common
import Hive.Impl.WebSocket ()

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Serialize
import Network.Socket

import qualified Network.WebSockets as WS

-- Quick-and-dirty main that never dies, but only plays out one game.
main :: IO ()
main = do
    sock <- WS.makeListenSocket "127.0.0.1" 6789


    putStrLn "waiting for player 1..."
    (s1, _) <- accept sock
    p1 <- WS.acceptRequest =<< WS.makePendingConnection s1 WS.defaultConnectionOptions

    putStrLn "waiting for player 2..."
    (s2, _) <- accept sock
    p2 <- WS.acceptRequest =<< WS.makePendingConnection s2 WS.defaultConnectionOptions

    runHive (wsShim p1) (wsShim p2)

-- | Receive encoded actions over a WebSocket connection and embed them in the
-- Hive monad.
wsShim :: WS.Connection -> Game -> Hive IO ()
wsShim conn game = do
    liftIO (WS.sendBinaryData conn (encode game))
    loop
  where
    loop = do
        bytes <- liftIO (WS.receiveData conn)
        case decode bytes of
            Left err -> liftIO (putStrLn err)
            Right action -> do
                result <-
                    case action of
                        Place bug idx -> makePlacement bug idx
                        Move i0 is -> makeMove i0 is
                liftIO (WS.sendBinaryData conn (encode result))
                case result of
                    Just (GameOver _) -> liftIO (gracefulClose conn)
                    _ -> loop

gracefulClose :: WS.Connection -> IO ()
gracefulClose conn = do
    WS.sendClose conn ("" :: ByteString)
    -- Wait up to 5 seconds for the other party to send a close message.
    _ <- race waitForClose (threadDelay 5000000)
    pure ()
  where
    waitForClose =
        catchJust
            (\case
                WS.ConnectionClosed -> Just ()
                _ -> Nothing)
            (forever (void (WS.receiveDataMessage conn)))
            (\_ -> pure ())
