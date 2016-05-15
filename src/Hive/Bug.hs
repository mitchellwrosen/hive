module Hive.Bug where

import Mitchell.Prelude

import Data.Aeson

-- | Bug enum.
data Bug
  = Ant
  | Grasshopper
  | Spider
  | Beetle
  | Queen
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)
