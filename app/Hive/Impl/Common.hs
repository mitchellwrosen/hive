module Hive.Impl.Common
    ( Action(..)
    ) where

import Data.HexBoard (BoardIndex)
import Hive.Bug

import Control.Applicative
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)

import qualified Data.List.NonEmpty as NonEmpty

data Action
    = Place Bug BoardIndex
    | Move BoardIndex (NonEmpty BoardIndex)

-- Crappy manually-written instances because there's no
-- instance ToJSON/FromJSON (NonEmpty a)

instance ToJSON Action where
  toJSON (Place bug idx) = toJSON (Number 0, bug, idx)
  toJSON (Move idx idxs) = toJSON (Number 1, idx, NonEmpty.toList idxs)

instance FromJSON Action where
  parseJSON v = parsePlace <|> parseMove
   where
    parsePlace = do
      (Number 0, bug, idx) <- parseJSON v
      pure (Place bug idx)

    parseMove = do
      (Number 1, idx, idxs) <- parseJSON v
      pure (Move idx (NonEmpty.fromList idxs))
