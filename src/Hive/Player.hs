module Hive.Player where

import Data.Aeson
import GHC.Generics (Generic)

-- | Nothing means the players tied.
type Winner = Maybe Player

-- | Player enum.
data Player
  = P1
  | P2
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)


nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1