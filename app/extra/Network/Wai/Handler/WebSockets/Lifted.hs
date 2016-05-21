module Network.Wai.Handler.WebSockets.Lifted where

import Mitchell.Prelude

import qualified Network.Wai.Lifted as Wai

import Control.Monad.Trans.Unlift

import qualified Network.Wai.Handler.WebSockets as Wai.WebSockets
import qualified Network.WebSockets             as WebSockets

websocketsOr
  :: MonadBaseUnlift IO m
  => WebSockets.ConnectionOptions
  -> (WebSockets.PendingConnection -> m ())
  -> Wai.Application m
  -> Wai.Application m
websocketsOr opts server app = \req resp -> do
  UnliftBase unlift <- askUnliftBase

  let serverIO :: WebSockets.PendingConnection -> IO ()
      serverIO = unlift . server

      appIO :: Wai.Application IO
      appIO = \req' resp' ->
        unlift (app req' (liftBase . resp'))

      respIO :: Wai.Response -> IO Wai.ResponseReceived
      respIO = unlift . resp

  liftBase (Wai.WebSockets.websocketsOr opts serverIO appIO req respIO)
