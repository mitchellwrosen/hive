{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Mitchell.Prelude -- TODO: lifted-base

import Hive

import qualified Network.Wai.Lifted                    as Wai
import qualified Network.Wai.Handler.Warp.Lifted       as Wai.Warp
import qualified Network.Wai.Handler.WebSockets.Lifted as Wai.WebSockets
import qualified Network.WebSockets.Lifted             as WebSockets

import Control.Exception          (AsyncException(ThreadKilled))
import Control.Lens
import Control.Monad.Log
import Control.Monad.Trans.Free   (FreeT)
import Control.Monad.Trans.Unlift

import qualified Data.Text.Encoding as Text
import qualified Data.Aeson         as Aeson
import qualified Network.HTTP.Types as HTTP


type RoomMap = Map ByteString Room

data Room = Room
  { roomPlayer1 :: MVar ThreadId
  , roomPlayer2 :: MVar ThreadId
  , roomGame    :: TVar GameState
  , roomLock    :: MVar ()
  }

-- data Client = Client
--   { clientName   :: ByteString
--   , clientThread :: ThreadId
--   , clientChan   ::

data Message
  = ActionMessage ActionMessage
  | ChatMessage ChatMessage

data ActionMessage = ActionMsg Action
data ChatMessage = ChatMsg Text


main :: IO ()
main = do
  p1   <- newEmptyMVar
  p2   <- newEmptyMVar
  game <- newTVarIO (GameActive initialGame)
  lock <- newEmptyMVar
  room <- newIORef (Room p1 p2 game lock)

  runLoggingT (main' room) putStrLn

main' :: (MonadLog Text m, MonadIO m, MonadBaseUnlift IO m) => IORef Room -> m ()
main' = undefined
-- main' room = do
--   logMessage ("Binding server to port " ++ show port)
--   Wai.Warp.run port (Wai.WebSockets.websocketsOr opts (wsApp room) httpApp)
--  where
--   port :: Wai.Warp.Port
--   port = 6789
--
--   opts :: WebSockets.ConnectionOptions
--   opts = WebSockets.defaultConnectionOptions
--
-- wsApp
--   :: forall m.
--      (MonadLog Text m, MonadIO m, MonadBase IO m)
--   => IORef Room
--   -> WebSockets.PendingConnection
--   -> m ()
-- wsApp room_ref pending_conn = do
--   logMessage ("Pending connection: " ++ show (WebSockets.pendingRequest pending_conn))
--
--   room@Room{..} <- liftBase (readIORef room_ref)
--
--   conn <- WebSockets.acceptRequest pending_conn
--
--   joinRoom room >>= \case
--     Just p -> do
--       logMessage ("Accepted " ++ show p)
--
--       WebSockets.sendLog conn ("You are " ++ show p) lbs2t
--
--       -- Player 1 blocks, waiting for player 2 to join
--       case p of
--         P1 -> liftBase (takeMVar roomLock)
--         P2 -> liftBase (putMVar roomLock ())
--
--       -- There's no way for the game to be over by either player's first turn,
--       -- so this partial pattern match is sound.
--       GameActive game0 <-
--         atomicallySTM (readTVarCheck (gameConcerns p) roomGame)
--
--       logMessage ("Sending initial game to " ++ show p)
--       WebSockets.sendLog conn (Aeson.encode game0) lbs2t
--
--       let loop :: Game -> Hive m () -> m ()
--           loop game player =
--             stepHive game player  >>= \case
--               -- The player sent un-decodable bytes, and we aborted. Kill the
--               -- other player.
--               Nothing -> do
--                 other_tid <-
--                   -- We rely on readMVar being atomic here (base 4.7+)
--                   case p of
--                     P1 -> io (readMVar roomPlayer2)
--                     P2 -> io (readMVar roomPlayer1)
--                 io (throwTo other_tid ThreadKilled)
--
--               -- If we've won, stepHive already took care of running the side
--               -- effects of the current player. So, just write to the game state
--               -- TVar to inform the other player and spectators, then end the
--               -- websocket connection by falling out of the loop.
--               Just (Left winner) ->
--                 atomicallySTM (writeTVar roomGame (GameOver winner))
--
--               -- We've made a move. Write to the game state TVar to inform the
--               -- other player and spectators, then read the same TVar until it's
--               -- our turn again (or the other player made a winning move).
--               Just (Right (game', player')) -> do
--                 assertM (view gamePlayer game' == nextPlayer p)
--
--                 atomicallySTM (writeTVar roomGame (GameActive game'))
--
--                 atomicallySTM (readTVarCheck (gameConcerns p) roomGame) >>= \case
--                   -- If the other player won, step this player one more time
--                   -- to cause the side-effect of sending the GameOver message
--                   GameOver winner ->
--                     stepPlayer (player' (GameOver winner))
--
--                   GameActive game'' ->
--                     loop game'' (player' (GameActive game''))
--
--       loop game0 (wsShim p conn)
--
--     Nothing -> do
--       logMessage "Rejecting spectator"
--       WebSockets.sendLog conn "Room is full" lbs2t
--
--
-- -- Attempt to join a room as player 1, then player 2
-- joinRoom :: MonadBase IO m => Room -> m (Maybe Player)
-- joinRoom Room{..} = liftBase $ do
--   tid <- myThreadId
--   tryPutMVar roomPlayer1 tid >>= \case
--     True  -> pure (Just P1)
--     False ->
--       tryPutMVar roomPlayer2 tid >>= \case
--         True  -> pure (Just P2)
--         False -> pure Nothing
--
--
-- -- | Receive encoded actions over a WebSocket connection and embed them in the
-- -- Hive monad.
-- wsShim
--   :: (MonadLog Text m, MonadBase IO m)
--   => Player
--   -> WebSockets.Connection
--   -> Hive m ()
-- wsShim (show -> p) conn = loop
--  where
--   loop = do
--     lift (logMessage ("Receiving bytes from " ++ p))
--     bytes <- liftBase (WebSockets.receiveData conn)
--     lift (logMessage ("Received bytes from " ++ p))
--
--     case Aeson.decode bytes of
--       Nothing ->
--         lift (logMessage ("Error decoding action from " ++ p))
--
--       Just action -> do
--         result <- hiveAction action
--         lift (WebSockets.sendLog conn (Aeson.encode result) lbs2t)
--
--         case result of
--           Left err             -> loop
--           Right (GameActive _) -> loop
--           Right (GameOver _)   -> pure ()
--
--
-- gracefulClose :: MonadBase IO m => WebSockets.Connection -> m ()
-- gracefulClose conn = do
--   WebSockets.sendClose conn
--   -- Wait up to 5 seconds for the other party to send a close message.
--   liftBase (race_ waitForClose (threadDelay 5000000))
--  where
--   waitForClose =
--     catchJust
--       (\case
--         WebSockets.ConnectionClosed -> Just ()
--         _ -> Nothing)
--       (forever (void (WebSockets.receiveDataMessage conn)))
--       pure
--
-- httpApp :: Wai.Application m
-- httpApp req resp = do
--   resp (Wai.responseLBS HTTP.status400 [] "Hello, stranger")
--
-- lbs2t :: LByteString -> Text
-- lbs2t = Text.decodeUtf8 . cs
--
-- --------------------------------------------------------------------------------
-- -- Orphan instances
--
-- instance (Functor f, MonadBase IO m) => MonadBase IO (FreeT f m) where
--   liftBase = lift . liftBase
--   {-# INLINE liftBase #-}
