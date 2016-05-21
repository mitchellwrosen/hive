{-# LANGUAGE ScopedTypeVariables #-}

module Hive
  ( stepHive
  , runHive
  , module Control.Monad.Trans.Class
  , module Data.HexBoard
  , module Hive.Board
  , module Hive.Bug
  , module Hive.Game
  , module Hive.Monad
  , module Hive.Player
  , module Hive.Tile
  ) where

import Mitchell.Prelude

import Data.HexBoard
import Hive.Board
import Hive.Bug
import Hive.Game
import Hive.Monad
import Hive.Player
import Hive.Tile

import Control.Lens
import Control.Monad.Trans.Free
import Control.Monad.Trans.Class
import Data.List.NonEmpty        (NonEmpty(..))

import qualified Data.List          as List
import qualified Data.List.Extra    as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector        as Vector

--------------------------------------------------------------------------------
-- Hive interpreter

-- | Step the Hive game forward. Assumes the given Game is not already won, and
-- is otherwise in some sort of sensible state.
--
-- Returns Left if the given move was invalid, and Right with the updated game
-- state and continuation if the given move was valid.
stepHive
  :: forall m.
     Monad m
  => Game
  -> HiveF (Hive m ())
  -> m (Either (Hive m ()) (GameState, GameState -> Hive m ()))
stepHive game = \case
  MakePlacement bug board_idx k -> do
    result <- runExceptT $ do
      let neighbors :: [Cell]
          neighbors = map snd (boardNeighbors board_idx board)

          has_opponent_neighbor :: Bool
          has_opponent_neighbor = any (\c -> cellOwner c == Just opponent) neighbors

          has_occupied_neighbor :: Bool
          has_occupied_neighbor = not (all null neighbors)

      -- The current player must have the bug in supply.
      when (bug `notElem` view gameBugs game) $
        throwError "No such bug"

      -- Queen must be placed by move 4. That is, we may not place a non-queen
      -- on turn 4 with a queen in the supply.
      when (bug /= Queen &&
            Queen `elem` view gameBugs game &&
            view gamePlaced game == 3) $
        throwError "Must place Queen by turn 4"

      case preview (ix board_idx) board of
        -- Index must be in bounds.
        Nothing -> throwError "Index out of bounds"
        -- Cell must not be occupied.
        Just (_:_) -> throwError "Cell occupied"
        _ -> pure ()

      case view gamePlaced game + view gamePlaced' game of
        -- The first tile placed has no restrictions (we would have already
        -- thrown an error if it was not placed at index (0, 0))
        0 -> pure ()
        -- The second tile must have at least one occupied neighbor (assumed to
        -- be the opponent's).
        1 ->
          when (not has_occupied_neighbor) $
            throwError "Must place tile adjacent to the hive"
        -- All other tiles must have at least one occupied neighbor, none of
        -- which can belong to the opponent.
        _ -> do
          when (not has_occupied_neighbor) $
            throwError "Must place tile adjacent to the hive"

          when has_opponent_neighbor $
            throwError "May not place tile adjacent to opponent's tile"

      let updateGame :: Game -> Game
          updateGame =
              over gameBoard   f
            . over gamePlayer  nextPlayer
            . set  gameBugs    (view gameBugs' game)
            . set  gameBugs'   (List.delete bug (view gameBugs game))
            . set  gamePlaced  (view gamePlaced' game)
            . set  gamePlaced' (view gamePlaced game + 1)
           where
            -- Poke the new tile into place, then possibly grow the board in
            -- all 4 directions to accomodate new viable cells to play at.
            f :: Board -> Board
            f = growBoard board_idx
              . over (ix board_idx) (\[] -> [Tile (game^.gamePlayer) bug])

      pure (updateGame game)

    case result of
      Left err    -> wasInvalidMove err k
      Right game' -> wasValidMove board_idx game' k

  MakeMove src path k -> do
    let dst :: BoardIndex
        dst = NonEmpty.last path

    result <- runExceptT $ do
      -- The queen must be placed before moving any tiles.
      when (Queen `elem` view gameBugs game) $ do
        throwError "May not move a tile before placing the Queen"

      -- The index of the first cell must be in bounds.
      cell <-
        case preview (ix src) board of
          Nothing   -> throwError "Index out of bounds"
          Just cell -> pure cell

      -- The source index must be occupied.
      (t@(Tile p bug), ts) <-
        case cell of
          [] -> throwError "No tile at index"
          (t:ts) -> pure (t,ts)

      -- The tile at the source index must belong to the current player.
      when (p /= view gamePlayer game) $
        throwError "May not move opponent's tile"

      validateMove bug src path board

      let updateGame :: Game -> Game
          updateGame =
              over gameBoard   f
            . over gamePlayer  nextPlayer
            . set  gameBugs    (view gameBugs' game)
            . set  gameBugs'   (view gameBugs game)
            . set  gamePlaced  (view gamePlaced' game)
            . set  gamePlaced' (view gamePlaced game)
           where
            f :: Board -> Board
            f = growBoard dst
              . over (ix dst) (t:)
              . set (ix src) ts

      pure (updateGame game)

    case result of
      Left err    -> wasInvalidMove err k
      Right game' -> wasValidMove dst game' k

 where
  -- Given the updated game, check to see if the game's over; construct and
  -- return the appropriate GameState.
  wasValidMove
    :: BoardIndex -- Where this move landed
    -> Game
    -> (Either Text GameState -> Hive m ())
    -> m (Either (Hive m ()) (GameState, GameState -> Hive m ()))
  wasValidMove modified_idx game' k = do
    let game_state =
          maybe (GameActive game')
                GameOver
                (boardWinner modified_idx (view gameBoard game'))
    pure (Right (game_state, k . Right))

  wasInvalidMove
    :: Text
    -> (Either Text GameState -> Hive m ())
    -> m (Either (Hive m ()) (GameState, GameState -> Hive m ()))
  wasInvalidMove err k = pure (Left (k (Left err)))

  board :: Board
  board = view gameBoard game

  opponent :: Player
  opponent = nextPlayer (view gamePlayer game)


runHive
  :: Monad m
  => (Game -> Hive m ()) -- Player 1
  -> (Game -> Hive m ()) -- Player 2
  -> m (Maybe Winner)    -- Nothing if some player exited early
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
  board = HexBoard (Vector.singleton (Vector.singleton [])) Even

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
  :: forall m. Monad m
  => Game                     -- Game board
  -> Hive m ()                -- Current player logic
  -> (GameState -> Hive m ()) -- Next player logic, waiting for board resulting
                              --   from current player's move
  -> m (Maybe Winner)
runHive' game cur_player next_player = do
  runFreeT cur_player >>= \case
    -- The current player's logic has ended, so we have nothing left to
    -- interpret. This shouldn't happen with well-written players.
    Pure () -> pure Nothing

    Free move ->
      stepHive game move >>= \case
        -- The current player made an invalid move. Loop without swapping
        -- players.
        Left cur_player' -> runHive' game cur_player' next_player

        -- The current player made a valid move.
        Right (game_state, cur_player') ->
          case game_state of
            -- If the game's not over yet, swap players and recurse.
            GameActive game' ->
              runHive' game' (next_player game_state) cur_player'

            -- If the game's over, run side-effects of player logic one last
            -- time (to inform them of the game ending), but don't bother
            -- continuing to interpret the AST. An oddly written player may
            -- attempt to make a move after the game is said to be over, but we
            -- don't care, we just stop interpreting.
            GameOver winner -> do
              _ <- runFreeT $ do
                cur_player' game_state
                next_player game_state
              pure (Just winner)


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
validateMove
  :: MonadError Text m
  => Bug
  -> BoardIndex
  -> NonEmpty BoardIndex
  -> Board
  -> m ()

-- "One Hive Rule".
--
-- Note that this only checks the number of strongly connected components that
-- result from removing the piece from the board, so a piece removed from the
-- edge of the board will pass this check, even though it might become its own
-- island if moved to its destination.
--
-- So, this implements more like 75% of the "One Hive Rule".
--
-- (unsafeTail is safe here per the precondition.)
validateMove _ src _ board
  | boardNumSCCs (not . null) (over (ix src) unsafeTail board) /= 1 =
      throwError "Move violates the One Hive Rule"

-- An ant slides around any number of cells.
validateMove Ant i0 (i1 :| is) board =
  unless (pieceCanSlide i0 (i1:is) board) $
    throwError "Ant cannot move in such a way"

-- A grasshopper hops over one or more occupied cells in a straight line.
validateMove Grasshopper i0 (i1 :| (i2:is)) board = do
  -- All cells along the path except for the last are occupied.
  when (any (cellIsUnoccupied board) path) $
    throwError "Grasshopper may only hop over occupied cells"

  -- The last cell is unoccupied.
  when (cellIsOccupied board dst) $
    throwError "Grasshopper must hop to an empty cell"

  -- Path forms a straight line, i.e. the adjacency relationship between
  -- consecutive cells is the same throughout. Note that this depends on
  -- the parity of the board, and is not as simple as taking the integer
  -- differences between the indices.
  unless (formsStraightLine (i0:i1:i2:is)) $
    throwError "Grasshopper must hop in a straight line"
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
validateMove Spider i0 (i1 :| [i2,i3]) board = do
  unless (pieceCanSlide i0 [i1,i2,i3] board) $
    throwError "Spider cannot move in such a way"

  unless (List.nub [i0,i1,i2,i3] == [i0,i1,i2,i3]) $
    throwError "Spider cannot backtrack"

-- A beetle moves to an adjacent tile with at least one neighbor that wasn't
-- its original position. This prevents a beetle from popping off the edge of
-- the hive and becoming its own island.
validateMove Beetle i0 (i1 :| []) board = do
  unless (cellsAreAdjacent board i0 i1) $ do
    throwError "Beetle must move to an adjacent cell"

  when (occupied_neighbors == [] || occupied_neighbors == [i0]) $
    throwError "Move violates the One Hive Rule"
 where
  occupied_neighbors :: [BoardIndex]
  occupied_neighbors = map fst (occupiedNeighbors board i1)

-- A queen slides one cell.
validateMove Queen i0 (i1 :| []) board =
  unless (pieceCanSlide i0 [i1] board) $
    throwError "Queen cannot slide in such a way"

validateMove _ _ _ _ = throwError "Invalid move"
