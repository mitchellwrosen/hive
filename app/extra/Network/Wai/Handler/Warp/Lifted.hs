module Network.Wai.Handler.Warp.Lifted
  ( run
  , Wai.Warp.Port
  ) where

import Mitchell.Prelude

import qualified Network.Wai.Lifted as Wai

import Control.Monad.Trans.Unlift

import qualified Network.Wai.Handler.Warp as Wai.Warp

run :: MonadBaseUnlift IO m => Wai.Warp.Port -> Wai.Application m -> m ()
run port app = do
  unlift <- askRunBase

  let appIO :: Wai.Application IO
      appIO = \req resp ->
        unlift (app req (liftBase . resp))

  liftBase (Wai.Warp.run port appIO)
