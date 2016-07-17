{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Mitchell.Prelude

import Application

import Hive.WebSocketServer.State

import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as Wai.WebSockets
import qualified Network.WebSockets             as WebSockets

main :: IO ()
main = do
  state <- initialServerState
  Warp.run port (Wai.WebSockets.websocketsOr opts (app state) httpApp)
 where
  opts :: WebSockets.ConnectionOptions
  opts = WebSockets.defaultConnectionOptions

  port :: Warp.Port
  port = 6789

httpApp :: Wai.Application
httpApp _ resp = resp (Wai.responseLBS HTTP.status400 [] "")
