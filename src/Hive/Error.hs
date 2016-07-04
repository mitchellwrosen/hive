module Hive.Error
  ( HiveError(..)
  , displayHiveError
  ) where

import Mitchell.Prelude

import Data.HexBoard (BoardIndex)
import Hive.Bug


data HiveError
  = OneHiveRule
  | FreedomToMoveRule BoardIndex BoardIndex
  | NoSuchBug Bug [Bug]
  | NoQueenByTurn4
  | IndexOutOfBounds BoardIndex
  | PlaceOnOccupiedCell
  | PlaceNextToOpponent
  | QueenNotYetPlaced
  | MoveSrcNoTile
  | MoveSrcOpponentTile
  | NonAdjacentMove BoardIndex BoardIndex
  | SlideToOccupiedCell BoardIndex
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

displayHiveError :: HiveError -> Text
displayHiveError = \case
  OneHiveRule           -> "Move violates the One Hive Rule"
  FreedomToMoveRule i j -> "Cannot slide from " ++ show i ++ " to " ++ show j ++ " per the Freedom to Move Rule"
  NoSuchBug bug bugs    -> "No " ++ show bug ++ " in " ++ show bugs
  NoQueenByTurn4        -> "Must place Queen on or before turn 4"
  IndexOutOfBounds i    -> "Index " ++ show i ++ " out of bounds"
  PlaceOnOccupiedCell   -> "Cannot place bug on occupied cell"
  PlaceNextToOpponent   -> "Cannot place bug next to opponent's cell"
  QueenNotYetPlaced     -> "Cannot move a bug before the Queen is placed"
  MoveSrcNoTile         -> "Cannot move no bug"
  MoveSrcOpponentTile   -> "Cannot move opponent's bug"
  NonAdjacentMove i j   -> "Cannot move from " ++ show i ++ " to " ++ show j
  SlideToOccupiedCell i -> "Cannot move to occupied cell at " ++ show i
  GrasshopperMoveLength -> "Grasshopper must hop over at least one cell"
  HopOverUnoccupiedCell -> "Grasshopper may only hop over occupied cells"
  HopToOccupiedCell     -> "Grasshopper must hop to unoccupied cell"
  HopInCrookedLine      -> "Grasshopper must hop in a straight line"
  SpiderMoveLength      -> "Spider must move exactly three cells"
  SpiderBacktrack       -> "Spider may not backtrack"
  BeetleMoveLength      -> "Beetle may only move one cell"
  QueenMoveLength       -> "Queen may only move one cell"
