module Main where

import Mitchell.Prelude

import Hive
import Hive.Impl.Common
import Hive.Impl.Console.Player

import Control.Monad.Trans.Free
import System.Console.Haskeline

import qualified Data.Aeson         as Aeson
import qualified Network.WebSockets as WS

main :: IO ()
main = WS.runClient "127.0.0.1" 6789 "/" $ \conn -> do
  runInputT defaultSettings (wsShim conn (consolePlayer "Bob"))

wsShim :: forall m. MonadIO m => WS.Connection -> (Game -> Hive m ()) -> m ()
wsShim conn player = do
  putStrLn "Waiting for game"
  bytes <- liftIO (WS.receiveData conn)
  putStrLn "Received game"
  case Aeson.decode bytes of
    Nothing -> putStrLn "Error decoding game from server"
    Just game -> go (player game)
 where
  go :: Hive m () -> m ()
  go = runFreeT >=> \case
    Pure _ -> pure ()
    Free instr -> do
      let (action, k) =
            case instr of
              MakePlacement bug idx k0 -> (Place bug idx, k0)
              MakeMove i0 is k0 -> (Move i0 is, k0)
      liftIO (WS.sendBinaryData conn (Aeson.encode action))
      bytes <- liftIO (WS.receiveData conn)
      case Aeson.decode bytes of
        Nothing -> putStrLn "Error decoding game state from server"
        Just state -> go (k state)
