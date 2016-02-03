module Hive.Impl.Common
    ( Action(..)
    ) where

import Hive.Types (BoardIndex, Bug)

import Data.List.NonEmpty (NonEmpty)

data Action
    = Place Bug BoardIndex
    | Move BoardIndex (NonEmpty BoardIndex)
