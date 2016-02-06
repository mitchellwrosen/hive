{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hive.Impl.Console.WebSocket where

import Hive
import Hive.Impl.Common

import Data.List.NonEmpty (NonEmpty)
import Data.Serialize

import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Orphan instances

instance Serialize Action    where
instance Serialize Bug       where
instance Serialize Game      where
instance Serialize GameState where
instance Serialize Parity    where
instance Serialize Player    where
instance Serialize Tile      where
instance Serialize Winner    where
instance Serialize a => Serialize (NonEmpty a)

instance Serialize a => Serialize (HexBoard a) where
    put (HexBoard tiles parity) = do
        put (V.toList (V.map V.toList tiles))
        put parity

    get = HexBoard
        <$> (V.fromList . map V.fromList <$> get)
        <*> get
