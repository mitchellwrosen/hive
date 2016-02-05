{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hive
    ( runHive
    , module Control.Monad.Trans.Class
    , module Hive.Monad
    , module Hive.Types
    ) where

import Hive.Monad
import Hive.Types

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Data.List.NonEmpty        (NonEmpty(..))
import Data.Maybe
import Data.Set                  (Set)

import qualified Data.List          as List
import qualified Data.List.Extra    as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as S
import qualified Data.Vector        as V

--------------------------------------------------------------------------------
-- Hive interpreter

runHive
    :: Monad m
    => (Game -> Hive m ())
    -> (Game -> Hive m ())
    -> m ()
runHive p1 p2 =
    runHive'
        game
        (p1 game)
        -- There's no way for the game to have a winner after just 1 turn,
        -- so this partial pattern match is ok.
        (\case
            GameActive game' -> p2 game'
            _ -> error "impossible")
  where
    game :: Game
    game = Game board P1 bugs bugs 0 0

    -- One empty tile stack
    board :: Board
    board = HexBoard (V.singleton (V.singleton [])) Even

    bugs :: [Bug]
    bugs =
        [ Ant, Ant, Ant
        , Grasshopper, Grasshopper, Grasshopper
        , Spider, Spider
        , Beetle, Beetle
        , Queen
        ]

-- Invariant: the first Hive monad represents the current player
-- (this lets us avoid a case-analysis of game^.gamePlayer)
runHive'
    :: forall m.
       Monad m
    => Game                     -- Game board
    -> Hive m ()                -- Current player logic
    -> (GameState -> Hive m ()) -- Next player logic, waiting for board resulting
                                --   from current player's move
    -> m ()
runHive' game cur_player next_player = do
    let board = game^.gameBoard

    runFreeT cur_player >>= \case
        -- The current player's logic has ended, so we have nothing left to
        -- interpret. This shouldn't happen with well-written players.
        Pure () -> pure ()

        Free (MakePlacement bug board_idx k)
            -- The current player must have the bug in supply.
            | bug `elem` game^.gameCurPlayerBugs

            -- Queen must be placed by move 4.
            , bug == Queen                            ||
              Queen `notElem` game^.gameCurPlayerBugs ||
              game^.gameCurPlayerPlaced /= 3

            -- The index must be in bounds, and must not have a tile on it.
            , cellIsUnoccupied board board_idx

            , let neighbors :: [Cell]
                  neighbors = map snd (boardNeighbors board_idx board)

                  has_opponent_neighbor :: Bool
                  has_opponent_neighbor = any (\c -> cellOwner c == Just opponent) neighbors

                  has_occupied_neighbor :: Bool
                  has_occupied_neighbor = not (all null neighbors)

            -- If this is the FIRST tile, ok.
            -- If this is the SECOND TILE, it must have at least one occupied neighbor.
            -- If this is the THIRD OR LATER tile, it must have at least one occupied
            -- neighbor, none of which can belong to the opponent.
            , num_bugs_placed == 0 ||
              num_bugs_placed == 1 && has_occupied_neighbor ||
              has_occupied_neighbor && not has_opponent_neighbor -> do

                -- Poke the new tile into place, then possibly grow the board in
                -- all 4 directions to accomodate new viable cells to play at.
                let f :: Board -> Board
                    f = possiblyGrowBoard board_idx
                      . over (ix board_idx) (\_ -> [Tile (game^.gamePlayer) bug])

                    game' :: Game
                    game' = game
                        & gameBoard           %~ f
                        & gamePlayer          %~ nextPlayer
                        & gameCurPlayerBugs   %~ List.delete bug
                        & gameCurPlayerPlaced %~ (+1)

                next board_idx game' k

            | otherwise -> invalidMove k

        Free (MakeMove src path k)
            -- The queen must have been played already.
            | Queen `notElem` game^.gameCurPlayerBugs

            -- The index of the first cell must be in bounds.
            , Just cell <- board ^? ix src

            -- There must be a tile there to move.
            , t@(Tile p bug) : ts <- cell

            -- The tile must belong to the current player.
            , p == game^.gamePlayer

            -- The move must be valid per the movement rules of the game.
            , isValidMove bug src path board -> do
                let dst :: BoardIndex
                    dst = NE.last path

                    -- Pop the tile off the source, push it onto the
                    -- destination, and possibly grow the board in all 4
                    -- directions.
                    f :: Board -> Board
                    f = possiblyGrowBoard dst
                      . over (ix dst) (t :)
                      . over (ix src) (\_ -> ts)

                    game' :: Game
                    game' = game
                        & gameBoard  %~ f
                        & gamePlayer %~ nextPlayer

                next dst game' k

            | otherwise -> invalidMove k
  where
    -- Given the updated game state, check to see if the game's over; if it
    -- isn't, recurse.
    next :: BoardIndex -> Game -> (Maybe GameState -> Hive m ()) -> m ()
    next modified_idx game' k =
        case boardWinner modified_idx (game'^.gameBoard) of
            -- No winner yet - loop with the updated game state and
            -- players.
            Nothing ->
                runHive'
                    game'
                    (next_player (GameActive game'))
                    (\result -> k (Just result))

            -- If the game's over, run side-effects of player logic one
            -- last time (to inform them of the game ending), but don't
            -- bother continuing to interpret the AST. An oddly written
            -- player may attempt to make a move after the game is said
            -- to be over, but we don't care, we just stop interpreting.
            Just winner -> do
                _ <- runFreeT $ do
                    k (Just (GameOver winner))
                    next_player (GameOver winner)
                pure ()

    -- Given an index that presumably just had a piece placed on it (or moved
    -- to), possibly grow the board if that index is on an edge of the board.
    possiblyGrowBoard :: BoardIndex -> Board -> Board
    possiblyGrowBoard (row, col) board =
        let
            w = board^.boardWidth
            h = board^.boardHeight

            f0, f1, f2, f3 :: Board -> Board
            f0 = if row == 0   then boardPrependRow else id
            f1 = if row == h-1 then boardAppendRow  else id
            f2 = if col == 0   then boardPrependCol else id
            f3 = if col == w-1 then boardAppendCol  else id
        in
            f0 (f1 (f2 (f3 board)))

    -- Recurse with the same game state, providing Nothing to the current
    -- player's continuation to indicate an invalid move.
    invalidMove :: (Maybe GameState -> Hive m ()) -> m ()
    invalidMove k = runHive' game (k Nothing) next_player

    -- Lens onto the current player's unplaced bugs.
    gameCurPlayerBugs :: Lens' Game [Bug]
    gameCurPlayerBugs =
        case game^.gamePlayer of
            P1 -> gameP1Bugs
            P2 -> gameP2Bugs

    -- Lens onto the current player's # of placed bugs.
    gameCurPlayerPlaced :: Lens' Game Int
    gameCurPlayerPlaced =
        case game^.gamePlayer of
            P1 -> gameP1Placed
            P2 -> gameP2Placed

    opponent :: Player
    opponent = nextPlayer (game^.gamePlayer)

    num_bugs_placed :: Int
    num_bugs_placed = game^.gameP1Placed + game^.gameP2Placed

-- | Is this a valid move?
--
-- Checks the following conditions:
--
--     - This move doesn't violate the "One Hive Rule" (the tiles must always be
--       part of the same strongly connected component, even during movement).
--
--     - This move doesn't violate the "Freedom to Move Rule" (a piece must be
--       able to slide freely along its path).
--
--     - The per-bug movement rules are adhered to.
--
-- However, this function assumes that the given bug is indeed allowed to move
-- per other game rules, such as whose turn it is, whether or not the queen has
-- been placed yet, and whether or not any beetles are prohibiting movement.
--
-- Precondition: the source tile has at least one bug on it (the bug being
-- moved).
isValidMove :: Bug -> BoardIndex -> NonEmpty BoardIndex -> Board -> Bool

-- "One Hive Rule".
--
-- Note that this only checks the number of strongly connected components that
-- result from removing the piece from the board, so a piece removed from the
-- edge of the board will pass this check, even though it might become its own
-- island if moved to its destination.
--
-- So, this implements more like 75% of the "One Hive Rule".
--
-- ('tail' is safe here per the precondition.)
isValidMove _ src _ board
    | boardNumSCCs (not . null) (over (ix src) tail board) /= 1 = False

-- An ant slides around any number of cells.
isValidMove Ant i0 (i1 :| is) board =
    pieceCanSlide i0 (i1:is) board

-- A grasshopper hops over one or more occupied cells in a straight line.
isValidMove Grasshopper i0 (i1 :| (i2:is)) board =
    -- All cells along the path except for the last are occupied.
    all (cellIsOccupied board) path &&
    -- The last cell is unoccupied.
    not (cellIsOccupied board dst) &&
    -- Path forms a straight line, i.e. the adjacency relationship between
    -- consecutive cells is the same throughout. Note that this depends on
    -- the parity of the board, and is not as simple as taking the integer
    -- differences between the indices.
    formsStraightLine (i0:i1:i2:is)
  where
    -- 'path' is the (bad) name for the cells in between the src and dst cell.
    Just (path, dst) = List.unsnoc (i1:i2:is)

    -- Precondition: list has at least three elements.
    formsStraightLine :: [BoardIndex] -> Bool
    formsStraightLine (j0:j1:j2:js) =
        boardAdjacency board j0 j1 == boardAdjacency board j1 j2 &&
        formsStraightLine (j1:j2:js)
    formsStraightLine _ = True -- should only be hit with 2 element list

-- A spider slides one cell at a time, and cannot backtrack.
isValidMove Spider i0 (i1 :| [i2,i3]) board =
    pieceCanSlide i0 [i1,i2,i3] board &&
    List.nub [i0,i1,i2,i3] == [i0,i1,i2,i3]

-- A beetle moves to an adjacent tile with at least one neighbor that wasn't
-- its original position. This prevents a beetle from popping off the edge of
-- the hive and becoming its own island.
isValidMove Beetle i0 (i1 :| []) board =
    cellsAreAdjacent board i0 i1 &&
    occupied_neighbors /= [] &&
    occupied_neighbors /= [i0]
  where
    occupied_neighbors :: [BoardIndex]
    occupied_neighbors = map fst (boardOccupiedNeighbors board i1)

-- A queen slides one cell.
isValidMove Queen i0 (i1 :| []) board =
    pieceCanSlide i0 [i1] board

isValidMove _ _ _ _ = False

-- | A piece can slide from one cell to another if:
--
--     - The destination is unoccupied.
--     - The source and destination share precicely one occupied neighbor.
--
-- Sharing zero occupied neighbors means they are too far apart. Sharing two
-- occupied neighbors means there's too small a gap to squeeze through.
--
-- Precondition: list of indices contains at least one element.
pieceCanSlide :: BoardIndex -> [BoardIndex] -> Board -> Bool
pieceCanSlide i0 is board =
    -- No cells after the first are occupied.
    all (cellIsUnoccupied board) is &&
    -- Each two cells along the path share exactly one occupied neighbor.
    go (map (S.fromList . boardOccupiedNeighbors board) (i0:is))
  where
    go :: [Set (BoardIndex, Cell)] -> Bool
    go [] = True -- should never be reached
    go [_] = True
    go (s0:s1:ss) = length (S.intersection s0 s1) == 1 && go (s1:ss)

-- | Two cells are adjacent if one of them is a neighbor of the other.
cellsAreAdjacent :: Board -> BoardIndex -> BoardIndex -> Bool
cellsAreAdjacent board src dst =
    dst `elem` map fst (boardNeighbors src board)

-- | Is this cell occupied by at least one tile?
cellIsOccupied :: Board -> BoardIndex -> Bool
cellIsOccupied board i = isJust (board ^? ix i . _head)

-- | Is this cell unoccuped (but still in bounds)?
cellIsUnoccupied :: Board -> BoardIndex -> Bool
cellIsUnoccupied board i = board ^? ix i == Just []

-- | Get a list of neighbor cells that have at least one tile in them.
--
-- Postcondition: each Cell is non-empty.
boardOccupiedNeighbors :: Board -> BoardIndex -> [(BoardIndex, Cell)]
boardOccupiedNeighbors board i = do
    n <- boardNeighbors i board
    guard (not (null (snd n)))
    pure n

--------------------------------------------------------------------------------
-- Misc. utils

cellOwner :: Cell -> Maybe Player
cellOwner xs = xs ^? ix 0 . tilePlayer

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- | Get the winner of this board, if any. Takes an index representing the last
-- index that a piece was either moved to or placed at as an optimization, since
-- a queen can only possibly be smothered at one of this cell's neighbors.
boardWinner :: BoardIndex -> Board -> Maybe Winner
boardWinner i0 board =
    go (catMaybes (map queenSmothered (boardNeighbors i0 board)))
  where
    queenSmothered :: (BoardIndex, Cell) -> Maybe Player
    queenSmothered (i1, cell)
        | Just (Tile player Queen) <- cell^?_last
        , length (boardOccupiedNeighbors board i1) == 6 =
            Just player
        | otherwise = Nothing

    go :: [Player] -> Maybe Winner
    go queens =
        case (elem P1 queens, elem P2 queens) of
            (True, True) -> Just BothWin
            (True,    _) -> Just P1Wins
            (   _, True) -> Just P2Wins
            _            -> Nothing

boardPrependRow :: Board -> Board
boardPrependRow board =
    board & boardTiles %~ V.cons (V.replicate (board^.boardWidth) [])

boardAppendRow :: Board -> Board
boardAppendRow board =
    board & boardTiles %~ flip V.snoc (V.replicate (board^.boardWidth) [])

boardPrependCol :: Board -> Board
boardPrependCol board =
    board & boardTiles.traverse %~ V.cons []
          & boardParity         %~ flipParity

boardAppendCol :: Board -> Board
boardAppendCol board =
    board & boardTiles.traverse %~ flip V.snoc []
