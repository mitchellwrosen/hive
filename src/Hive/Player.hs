module Hive.Player where

import Mitchell.Prelude

import Data.Aeson

-- | Nothing means the players tied.
type Winner = Maybe Player

-- | Player enum.
data Player
  = P1
  | P2
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1
