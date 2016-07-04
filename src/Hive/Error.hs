module Hive.Error where

import Mitchell.Prelude

import Data.HexBoard (BoardIndex)


data HiveError
  = OneHiveRule
  -- ^ The attempted placement or move would violate the One Hive Rule
  | FreedomToMoveRule
  -- ^ The attempted move would violate the Freedom to Move rule
  | NoSuchBug
  -- ^ A bug that is not in the supply was attempted to be placed
  | NoQueenByTurn4
  -- ^ The 4th non-Queen bug was attempted to be placed before the Queen
  | IndexOutOfBounds
  -- ^ A bug was attempted to be placed on or moved to an out-of-bounds index
  | CellOccupied
  -- ^ The target of a placement or move is occupied
  | PlacementNextToOpponent
  -- ^ The attempted placement was next to an opponent's cell
  | QueenNotYetPlaced
  -- ^ A move was attempted before the Queen has been placed
  | MoveSrcNoTile
  -- ^ A move was attempted from an empty cell
  | MoveSrcOpponentTile
  -- ^ A move was attempted on an opponent's tile
  | NonAdjacentMove BoardIndex BoardIndex
  -- ^ A non-adjacent move was attempted.
  | SlideToOccupiedCell
  -- ^ A sliding move into an occupied cell was attempted
  | SlideToNonAdjacentCell
  -- ^ A sliding move between non-adjacent cell was attempted
  | GrasshopperMoveLength
  -- ^ A < length 2 Grasshopper move was attempted
  | HopOverUnoccupiedCell
  -- ^ A hop over an unoccupied cell was attempted
  | HopToOccupiedCell
  -- ^ A hop to an occupied cell was attempted
  | HopInCrookedLine
  -- ^ A hop in a non-straight line was attempted
  | SpiderMoveLength
  -- ^ A non-length-3 Spider move was attempted
  | SpiderBacktrack
  -- ^ A backtracking Spider move was attempted
  | BeetleMoveLength
  -- ^ A non-length-1 Beetle move was attempted
  | QueenMoveLength
  -- ^ A non-length-1 Queen move was attempted
  deriving Show
