module Network.Wai.Lifted
  ( Application
  , Wai.Request
  , Wai.Response
  , Wai.ResponseReceived
  , Wai.responseLBS
  ) where

import Mitchell.Prelude

import qualified Network.Wai as Wai

type Application m
  = Wai.Request -> (Wai.Response -> m Wai.ResponseReceived) -> m Wai.ResponseReceived
