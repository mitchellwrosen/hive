{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Hive
import Hive.Impl.Common
import Hive.Impl.Console
import Hive.Impl.WebSocket ()

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Data.Serialize
import System.Console.Haskeline

import qualified Network.WebSockets as WS

main :: IO ()
main = WS.runClient "127.0.0.1" 6789 "/" $ \conn -> do
    runInputT defaultSettings (wsShim conn (consolePlayer "Bob"))

wsShim :: forall m. MonadIO m => WS.Connection -> (Game -> Hive m ()) -> m ()
wsShim conn player = do
    liftIO (putStrLn "Receiving game bytes")
    bytes <- liftIO (WS.receiveData conn)
    liftIO (putStrLn "Received game bytes")
    case decode bytes of
        Left err -> liftIO (putStrLn err)
        Right game -> go (player game)
  where
    go :: Hive m () -> m ()
    go = runFreeT >=> \case
        Pure () -> pure ()
        Free instr -> do
            let (action, k) =
                    case instr of
                        MakePlacement bug idx k0 -> (Place bug idx, k0)
                        MakeMove i0 is k0 -> (Move i0 is, k0)
            liftIO (WS.sendBinaryData conn (encode action))
            bytes <- liftIO (WS.receiveData conn)
            case decode bytes of
                Left err -> liftIO (putStrLn err)
                Right state -> go (k state)
