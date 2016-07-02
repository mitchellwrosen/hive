{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Mitchell.Prelude

import Hive
import Hive.Impl.Console.Player

import qualified Network.WebSockets.Lifted as WebSockets

import Control.Monad.Trans.Free
import System.Console.Haskeline hiding (catch)

import qualified Data.Aeson as Aeson


main :: IO ()
main = WebSockets.runClient "127.0.0.1" 6789 "/" $ \conn -> do
  runInputT defaultSettings (wsShim conn (consolePlayer "Bob"))

wsShim :: forall m. (MonadIO m, MonadBase IO m) => WebSockets.Connection -> (Game -> Hive m ()) -> m ()
wsShim conn player = do
  putStrLn "Waiting for message"
  WebSockets.receiveData conn >>= (print :: LByteString -> m ())

  putStrLn "Waiting for game"
  bytes <- WebSockets.receiveData conn
  case Aeson.decode bytes of
    Nothing -> putStrLn "Error decoding game from server"
    Just game -> do
      putStrLn ("Received game: " ++ show game)
      go (player game)
 where
  go :: Hive m () -> m ()
  go = runFreeT >=> \case
    Pure _ -> pure ()
    Free instr -> do
      let (action, k) =
            case instr of
              MakePlacement bug idx k0 -> (Place bug idx, k0)
              MakeMove i0 is k0 -> (Move i0 is, k0)
      putStrLn ("Sending action " ++ show action)
      io $
        WebSockets.send conn (Aeson.encode action)
        `catch`
        (\e -> do
          putStrLn ("Wtf, exception thrown by send: " ++ show (e :: SomeException)))

      putStrLn "Receiving response"
      bytes <- WebSockets.receiveData conn
      case Aeson.decode bytes of
        Nothing -> putStrLn "Error decoding response from server"
        Just response -> do
          putStrLn ("Received response: " ++ show response)
          go (k response)

--------------------------------------------------------------------------------
-- Orphan instances

instance MonadBase IO m => MonadBase IO (InputT m) where
  liftBase = lift . liftBase
