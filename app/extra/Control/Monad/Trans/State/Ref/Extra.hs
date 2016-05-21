module Control.Monad.Trans.State.Ref.Extra
  ( getStateCAS
  , putStateCAS
  , modifyStateCAS
  , module Control.Monad.Trans.State.Ref
  ) where

import Mitchell.Prelude

import Control.Monad.Trans.State.Ref
import Data.Atomics



getStateCAS :: (MonadTrans t, MonadBase IO m) => t (StateRefT IORef s m) (Ticket s)
getStateCAS = lift (StateRefT (\ref -> liftBase (readForCAS ref)))

putStateCAS
  :: (MonadTrans t, MonadBase IO m)
  => Ticket s
  -> s
  -> t (StateRefT IORef s m) (Bool, Ticket s)
putStateCAS ticket s = lift (StateRefT (\ref -> liftBase (casIORef ref ticket s)))

modifyStateCAS
  :: (Monad (t (StateRefT IORef s m)), MonadTrans t, MonadBase IO m)
  => (s -> s)
  -> t (StateRefT IORef s m) Bool
modifyStateCAS f = do
  ticket <- getStateCAS
  let s = peekTicket ticket
  fst <$> putStateCAS ticket (f $! s)
