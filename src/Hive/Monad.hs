{-# LANGUAGE DeriveFunctor #-}

module Hive.Monad where

import Hive.Types

import Data.List.NonEmpty       (NonEmpty)
import Control.Monad.Trans.Free

-- | Underlying player action functor.
data HiveF a
    -- Place a new tile.
    = MakePlacement
        -- The bug to place
        Bug
        -- The cell to place it at
        BoardIndex
        -- The continuation; gets Nothing if this was an invalid placement.
        (Maybe GameState -> a)

    -- Move an already-placed tile.
    | MakeMove
        -- Source cell, and non-empty path to the destination.
        BoardIndex
        (NonEmpty BoardIndex)
        -- The continuation; gets Nothing if this was an invalid move.
        (Maybe GameState -> a)
    deriving Functor

-- The Hive monad, where player actions and inner monad actions are
-- interleaved.
type Hive m a = FreeT HiveF m a

makePlacement
    :: Monad m
    => Bug
    -> BoardIndex
    -> Hive m (Maybe GameState)
makePlacement bug idx = liftF (MakePlacement bug idx id)

makeMove
    :: Monad m
    => BoardIndex
    -> NonEmpty BoardIndex
    -> Hive m (Maybe GameState)
makeMove src path = liftF (MakeMove src path id)
