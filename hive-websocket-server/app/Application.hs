module Application where

import Mitchell.Prelude

import Hive.WebSocketServer.Client
import Hive.WebSocketServer.Room
import Hive.WebSocketServer.State

import Control.Monad.Managed

import qualified Network.WebSockets as WebSockets

app :: ServerState -> WebSockets.PendingConnection -> IO ()
app state conn = do
  room <- readIORef (stateRoom state)
  tryBecomePlayer1 room conn

-- | Try to become player 1. If there's already a player 2, transition to
-- trying to become player 2. Otherwise, transition to becoming player 1.
tryBecomePlayer1 :: Room -> WebSockets.PendingConnection -> IO ()
tryBecomePlayer1 room conn =
  atomicallySTM (tryPutTMVar (roomPlayer1 room) Nothing) >>= \case
    True  -> becomePlayer1 room conn
    False -> undefined

-- | We've succeeded in putting Nothing into the player 1 TMVar, so proceed to
-- swap it out for a Just after accepting the client connection. Then,
-- transition to waiting for player 2 to join.
becomePlayer1 :: Room -> WebSockets.PendingConnection -> IO ()
becomePlayer1 room pconn = do
  conn   <- WebSockets.acceptRequest pconn
  client <- newClient "player1" conn
  atomicallySTM (swapTMVar (roomPlayer1 room) (Just client))

  -- Spawn the client recv thread, client send thread, and client server
  -- thread. If any of them end (either by exception or terminating naturally),
  -- the others will be canceled.
  --
  -- If we die unnaturally, set the room's TMVar back to Nothing, so the other
  -- player's thread can notice and die along with us.
  runManaged $ do
    a1 <- managed (withAsync (clientRecvLoop client))
    a2 <- managed (withAsync (clientSendLoop client))
    a3 <- managed (withAsync (waitForPlayer2 (roomPlayer2 room) client))

    let runThreads :: IO ()
        runThreads = atomicallySTM $
          waitSTM a1 <|>
          waitSTM a2 <|>
          waitSTM a3

        writeNothing :: IO ()
        writeNothing = atomicallySTM $ do
          swapTMVar (roomPlayer1 room) Nothing
          pure ()

    liftIO (runThreads `onException` writeNothing)

-- | Wait for Just to be written to the given TMVar. Nothing will be written
-- first, but we don't want to go on until player 2's connection has actually
-- been accepted, because we will continue to watch this TMVar to go back to
-- Nothing in the case that player 2 disconnects.
waitForPlayer2 :: TMVar (Maybe Client) -> Client -> IO ()
waitForPlayer2 p2 client = do
  retryUntilJust p2
  myTurn client
 where
  -- Wait for a TMVar to contain a Just.
  retryUntilJust :: TMVar (Maybe a) -> IO ()
  retryUntilJust tmv = atomicallySTM $ do
    mx <- readTMVar tmv
    checkSTM (isJust mx)

myTurn :: Client -> IO ()
myTurn client = undefined
