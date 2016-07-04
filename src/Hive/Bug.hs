module Hive.Bug
  ( Bug(..)
  , initialBugs
  ) where

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

initialBugs :: [Bug]
initialBugs =
  [ Ant, Ant, Ant
  , Grasshopper, Grasshopper, Grasshopper
  , Spider, Spider
  , Beetle, Beetle
  , Queen
  ]
