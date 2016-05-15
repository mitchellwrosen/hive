module Hive.Monad where

import Data.HexBoard (BoardIndex)
import Hive.Bug
import Hive.Game

import Data.List.NonEmpty       (NonEmpty)
import Data.Text                (Text)
import Control.Monad.Trans.Free

-- | Underlying player action functor.
data HiveF a
  -- Place a new tile.
  = MakePlacement
    -- The bug to place
    Bug
    -- The cell to place it at
    BoardIndex
    -- The continuation; gets Left if this was an invalid placement.
    (Either Text GameState -> a)

  -- Move an already-placed tile.
  | MakeMove
    -- Source cell, and non-empty path to the destination.
    BoardIndex
    (NonEmpty BoardIndex)
    -- The continuation; gets Left if this was an invalid move.
    (Either Text GameState -> a)
  deriving Functor

-- The Hive monad, where player actions and inner monad actions are
-- interleaved.
type Hive m a = FreeT HiveF m a

makePlacement
  :: Monad m
  => Bug
  -> BoardIndex
  -> Hive m (Either Text GameState)
makePlacement bug idx = liftF (MakePlacement bug idx id)

makeMove
  :: Monad m
  => BoardIndex
  -> NonEmpty BoardIndex
  -> Hive m (Either Text GameState)
makeMove src path = liftF (MakeMove src path id)
