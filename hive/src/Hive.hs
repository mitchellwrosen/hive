module Hive where

import Data.List.Zipper

--------------------------------------------------------------------------------
-- Types

-- | Player 1 or Player 2
data Player
    = P1
    | P2

-- | Bug type.
data Bug
    = Ant
    | Grasshopper
    | Spider
    | Beetle
    | Queen

-- | A tile is a Bug and belongs to a Player.
data Tile
    = Tile Player Bug

-- A stack of tiles, with the topmost tile at the head of the list.
type TileStack = [Tile]

-- | Direction from a tile.
data Adjacency
    = UpLeft
    | Up
    | UpRight
    | DownRight
    | Down
    | DownLeft
    | Center -- Directly on top, like a beetle

-- | Index into the board. Row, then column.
type Index = (Int, Int)

-- | A path through the game board that a piece takes.
--
-- Invariant: this list always contains at least two elements.
type Path = [Index]

-- | Hive game board, represented as a 2D array of vertical
-- stacks of hexagonal tiles.
--
-- Uses the "odd-q" vertical layout, as depected here:
-- http://www.redblobgames.com/grids/hexagons/#coordinates
type Board = [[TileStack]]

-- | A move from one tile to another.
data Move = Move Path Adjacency

-- | The placing of a new tile.
data Placement = Placement Tile Index Adjacency

-- | The winner of a game.
data Winner
    = P1Wins
    | P2Wins
    | BothWin

--------------------------------------------------------------------------------
-- Game logic

-- | Does this board have a winner?
gameWinner :: Board -> Maybe Winner
gameWinner board = Nothing

-- | Make the first move of the game, which is always valid.
makeFirstPlacement :: Tile -> Board
makeFirstPlacement tile = [[[tile]]]

-- | Place a new tile on a board. Returns Nothing if the placement was invalid.
makePlacement :: Placement -> Board -> Maybe Board
makePlacement placement board
    | isValidPlacement placement board = Just board
    | otherwise = Nothing

-- Determine if a placement is valid, which may not be the case for any of the
-- following reasons:
--
--     - The destination is not adjacent to any tile.
--     - The destination is adjacent to a tile stack of the opposite color.
--     - The destination is on top of the hive.
--
-- Note that some illegal placements are out of scope here because they require
-- game state. For example, the tile being placed may belong to the other
-- player. Also, a player may be attempting to place a fourth non-queen tile.
isValidPlacement :: Placement -> Board -> Bool
isValidPlacement _ _ = False

-- Make a move on a board. Returns Nothing if the move was invalid.
makeMove :: Move -> Board -> Maybe Board
makeMove move board
    | isValidMove move board = Just board
    | otherwise = Nothing

-- Determine if a move is valid, which may not be the case for any of the
-- following reasons:
--
--     - The source index is out of bounds.
--     - The source index has no tile on it.
--     - The move is illegal per the bug movement rules.
--
-- Note that some illegal moves are out of scope here, because they require
-- game state. For example, the source tile may belong to the other player.
-- Also, a player may be attempting to move a tile when s/he has not yet played
-- his/her queen.
isValidMove :: Move -> Board -> Bool
isValidMove _ _ = False
