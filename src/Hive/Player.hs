module Hive.Player
  ( Player(..)
  , Winner
  , nextPlayer
  ) where

import Mitchell.Prelude

import Data.Aeson


-- | Player enum.
data Player
  = P1
  | P2
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | Nothing means the players tied.
type Winner = Maybe Player

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1
