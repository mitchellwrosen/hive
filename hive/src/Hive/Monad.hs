{-# LANGUAGE DeriveFunctor #-}

module Hive.Monad where

import Hive.Types

import Control.Monad.Trans.Free

-- | Underlying player action functor.
data HiveF a
    -- Place a new tile.
    = MakePlacement
        Bug                    -- The bug to place
        BoardIndex             -- The cell to place it at
        (Maybe GameState -> a) -- The resulting board state (after opp. goes)
    -- Move an already-placed tile.
    | MakeMove
        [BoardIndex]           -- The path from source cell to dest. cell
        (Maybe GameState -> a) -- The resulting board state (after opp. goes)
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

makeMove :: Monad m => [BoardIndex] -> Hive m (Maybe GameState)
makeMove path = liftF (MakeMove path id)
