module Hive.Bug where

import Data.Aeson
import GHC.Generics (Generic)

-- | Bug enum.
data Bug
  = Ant
  | Grasshopper
  | Spider
  | Beetle
  | Queen
  deriving (Eq, Generic, Ord, Read, Show, FromJSON, ToJSON)
