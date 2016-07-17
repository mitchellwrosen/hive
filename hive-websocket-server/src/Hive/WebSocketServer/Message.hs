module Hive.WebSocketServer.Message where

import Mitchell.Prelude

import Hive.Action
import Hive.Game

import Data.Aeson

data InMessage
  = InMessageAction Action
  | InMessageUnknown ByteString

instance FromJSON InMessage where
  parseJSON = withObject "object" (\o ->
    o .: "type" >>=
      withText "text" (\case
        "action" -> map InMessageAction (o .: "action")))

data OutMessage
  = OutMessageGameState GameState

instance ToJSON OutMessage where
  toJSON = \case
    OutMessageGameState state ->
      object
        [ "type"       .= String "game_state"
        , "game_state" .= toJSON state
        ]
