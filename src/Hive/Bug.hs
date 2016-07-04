module Hive.Bug
  ( Bug(..)
  , initialBugs
  ) where

import Mitchell.Prelude

import Hive.Expansions

import Data.Aeson


-- | Bug enum.
data Bug
  = Ant
  | Grasshopper
  | Ladybug
  | Spider
  | Beetle
  | Queen
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

initialBugs :: UseLadybug -> [Bug]
initialBugs ladybug = concat
  [ replicate 3 Ant
  , replicate 3 Grasshopper
  , replicate 2 Spider
  , replicate 2 Beetle
  , [ Ladybug | ladybug ]
  , [ Queen ]
  ]
