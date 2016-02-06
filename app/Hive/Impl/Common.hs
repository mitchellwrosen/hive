{-# LANGUAGE DeriveGeneric #-}

module Hive.Impl.Common
    ( Action(..)
    ) where

import Hive.Types (BoardIndex, Bug)

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics       (Generic)

data Action
    = Place Bug BoardIndex
    | Move BoardIndex (NonEmpty BoardIndex)
    deriving (Generic)
