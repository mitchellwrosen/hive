{-# LANGUAGE TemplateHaskell #-}

module Hive.Game
  ( Game(..)
  , gameBoard
  , gamePlayer
  , gameBugs
  , gameBugs'
  , gamePlaced
  , gamePlaced'
  , GameState(..)
  , initialGame
  , gameOver
  , gameConcerns
  , stepGame
  ) where

import Mitchell.Prelude

import Data.HexBoard
import Hive.Action
import Hive.Board
import Hive.Bug
import Hive.Error
import Hive.Player
import Hive.Tile
import Utils         (adjacentPairs)

import Control.Lens
import Data.Aeson

import qualified Data.List          as List
import qualified Data.List.Extra    as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set           as Set
import qualified Data.Vector        as Vector


data Game = Game
    { _gameBoard   :: !Board  -- ^ Game board
    , _gamePlayer  :: !Player -- ^ Current player
    , _gameBugs    :: ![Bug]  -- ^ Current player's bugs
    , _gameBugs'   :: ![Bug]  -- ^ Next player's bugs
    , _gamePlaced  :: !Int    -- ^ # of bugs current player has placed
    , _gamePlaced' :: !Int    -- ^ # of bugs next player has placed
    } deriving (Eq, Show)
makeLenses ''Game

instance ToJSON Game where
  toJSON game = object
    [ ("board",   toJSON (view gameBoard   game))
    , ("player",  toJSON (view gamePlayer  game))
    , ("bugs",    toJSON (view gameBugs    game))
    , ("bugs'",   toJSON (view gameBugs'   game))
    , ("placed",  toJSON (view gamePlaced  game))
    , ("placed'", toJSON (view gamePlaced' game))
    ]

instance FromJSON Game where
  parseJSON = withObject "object" $ \o ->
    Game
      <$> o .: "board"
      <*> o .: "player"
      <*> o .: "bugs"
      <*> o .: "bugs'"
      <*> o .: "placed"
      <*> o .: "placed'"


-- | The state of a game: it's over, or it's active.
data GameState
  = GameOver Winner
  | GameActive Game
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


initialGame :: Game
initialGame = Game board P1 bugs bugs 0 0
 where
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

-- | Is the game over?
gameOver :: GameState -> Bool
gameOver (GameOver _) = True
gameOver _ = False

-- | Does this game concern this player? (i.e. is it over, or their turn?)
gameConcerns :: Player -> GameState -> Bool
gameConcerns _ (GameOver _) = True
gameConcerns player (GameActive game) =
  player == view gamePlayer game


-- | Step the game forward with the given action. This implmenents all of the
-- Hive game logic. The input game is assumed to be valid.
stepGame :: Action -> Game -> Either HiveError GameState
stepGame act game =
  case act of
    Place bug idx -> do
      checkHasBug bug bugs
      checkQueenByTurn4 bug game

      cell <- checkValidIndex idx board
      checkCellUnoccupied cell

      checkValidAdjacency idx game

      pure (placeBug bug idx game)

    Move src tl -> do
      checkQueenPlaced bugs

      let path :: [BoardIndex]
          path = src : NonEmpty.toList tl

      -- Check bug-agnostic things: every index is in bounds, every index is
      -- adjacent to its neighbors, and the source cell belongs to the current
      -- player
      cell:_ <- mapM (\i -> checkValidIndex i board) path
      bug <- checkValidMoveSrc cell player
      mapM_ (\(i,j) -> checkAdjacent i j board) (adjacentPairs path)

      -- Check bug-specific movement rules
      case bug of
        Ant         -> validateAntMove src tl board
        Grasshopper -> validateGrasshopperMove src tl board
        Spider      -> validateSpiderMove src tl board
        Beetle      -> validateBeetleMove tl
        Queen       -> validateQueenMove src tl board

      let dst :: BoardIndex
          dst = NonEmpty.last tl

      checkOneHiveRule src (NonEmpty.last tl) board

      pure (moveBug src dst game)
 where
  board :: Board
  board = view gameBoard game

  player :: Player
  player = view gamePlayer game

  bugs :: [Bug]
  bugs = view gameBugs game

-- | Update a game by placing a bug. Assumes that the placement is valid.
placeBug :: Bug -> BoardIndex -> Game -> GameState
placeBug bug idx game =
  case boardWinner idx board' of
    Nothing     -> GameActive game'
    Just winner -> GameOver winner
 where
  game' :: Game
  game' = updateGame game

  board' :: Board
  board' = view gameBoard game'

  player :: Player
  player = view gamePlayer game

  updateGame :: Game -> Game
  updateGame =
    -- Possibly grow the board in all 4 directions to accomodate new
    -- viable cells to play at.
      over gameBoard (growBoard idx)

    -- Poke the new tile into place. Partial pattern match on [] is okay
    -- because a placement onto an occupied cell should have been
    -- illegal.
    . over (gameBoard . ix idx)
        (\[] -> [Tile player bug])

    -- Swap the current player
    . over gamePlayer nextPlayer

    -- Swap gameBugs/gameBugs' and delete the placed bug from the
    -- current player's (the updated game's *next* player)
    . set gameBugs (view gameBugs' game)
    . set gameBugs' (List.delete bug (view gameBugs game))

    -- Swap gamePlaced/gamePlaced' and increment the current player's
    -- (updated game's *next* player)
    . set gamePlaced (view gamePlaced' game)
    . set gamePlaced' (view gamePlaced game + 1)

moveBug :: BoardIndex -> BoardIndex -> Game -> GameState
moveBug src dst game =
  case boardWinner dst board' of
    Nothing     -> GameActive game'
    Just winner -> GameOver winner
 where
  (t:ts) = view gameBoard game ! src

  game' :: Game
  game' = updateGame game

  board' :: Board
  board' = view gameBoard game'

  updateGame :: Game -> Game
  updateGame =
    -- Possibly grow the board in all 4 directions to accomodate new
    -- viable cells to play at.
    -- TODO: Possibly shrink board as well
      over gameBoard (growBoard dst)

    -- Remove the bug from src and add it to dst
    . over (gameBoard . ix dst) (t:)
    . set (gameBoard . ix src) ts

    -- Swap the current player
    . over gamePlayer nextPlayer

    -- Swap gameBugs/gameBugs'
    . set gameBugs (view gameBugs' game)
    . set gameBugs' (view gameBugs game)

    -- Swap gamePlaced/gamePlaced'
    . set gamePlaced (view gamePlaced' game)
    . set gamePlaced' (view gamePlaced game)

-- The current player must have the bug in supply.
checkHasBug :: Bug -> [Bug] -> Either HiveError ()
checkHasBug bug bugs =
  when (bug `notElem` bugs) $
    throwError NoSuchBug

-- Queen must be placed by move 4. That is, we may not place a non-queen
-- on turn 4 with a queen in the supply.
checkQueenByTurn4 :: Bug -> Game -> Either HiveError ()
checkQueenByTurn4 bug game =
  when (and [ bug /= Queen
            , Queen `elem` bugs
            , placed == 3
            ]) $
    throwError NoQueenByTurn4
 where
  bugs   = view gameBugs game
  placed = view gamePlaced game

-- Index must be in bounds. Returns the cell at the index, if valid.
checkValidIndex :: BoardIndex -> Board -> Either HiveError Cell
checkValidIndex idx board =
  case preview (ix idx) board of
    Nothing   -> throwError IndexOutOfBounds
    Just cell -> pure cell

checkCellUnoccupied :: Cell -> Either HiveError ()
checkCellUnoccupied = \case
  _:_ -> throwError CellOccupied
  _   -> pure ()

-- Depending on what turn it is, check that the placement's neighbors are of
-- valid colors.
checkValidAdjacency :: BoardIndex -> Game -> Either HiveError ()
checkValidAdjacency idx game =
  case placed of
    -- The first tile placed has no restrictions (except for in-bounds-ness
    -- which we check separately)
    0 -> pure ()

    -- The second tile must have at least one occupied neighbor (assumed to
    -- be the opponent's)
    1 ->
      when (not has_occupied_neighbor) $
        throwError OneHiveRule

    -- All other tiles must have at least one occupied neighbor, none of
    -- which can belong to the opponent.
    _ -> do
      when (not has_occupied_neighbor) $
        throwError OneHiveRule

      when has_opponent_neighbor $
        throwError PlacementNextToOpponent
 where
  placed = view gamePlaced game + view gamePlaced' game

  board :: Board
  board = view gameBoard game

  opponent :: Player
  opponent = nextPlayer (view gamePlayer game)

  neighbors :: [Cell]
  neighbors = map snd (boardNeighbors idx board)

  has_opponent_neighbor :: Bool
  has_opponent_neighbor = any (\c -> cellOwner c == Just opponent) neighbors

  has_occupied_neighbor :: Bool
  has_occupied_neighbor = not (all null neighbors)

-- The queen must be placed before moving any tiles.
checkQueenPlaced :: [Bug] -> Either HiveError ()
checkQueenPlaced bugs =
  when (Queen `elem` bugs) $
    throwError QueenNotYetPlaced

checkValidMoveSrc :: Cell -> Player -> Either HiveError Bug
checkValidMoveSrc cell player =
  case cell of
    [] -> throwError MoveSrcNoTile
    (t:_) -> do
      when (view tilePlayer t /= player) $
        throwError MoveSrcOpponentTile
      pure (view tileBug t)

checkAdjacent
  :: MonadError HiveError m
  => BoardIndex
  -> BoardIndex
  -> Board
  -> m ()
checkAdjacent i j board =
  case boardAdjacency board i j of
    Nothing -> throwError (NonAdjacentMove i j)
    Just _  -> pure ()

-- "One Hive Rule" - the number of strongly connected components of the board is
-- 1 both "during" movement (after the piece is lifted) and after movement.
--
-- Precondition: there is some tile at the given cell index (so unsafeTail is
-- safe)
checkOneHiveRule
  :: MonadError HiveError m
  => BoardIndex
  -> BoardIndex
  -> Board
  -> m ()
checkOneHiveRule src dst board = do
  when (sccs board' /= 1) $
    throwError OneHiveRule

  -- No need to check num SCCs of board after placement. Instead, just see if
  -- the destination cell has no neighbors after the src cell has been removed.
  when (Set.null (occupiedNeighborIndices dst board')) $
    throwError OneHiveRule
 where
  board' = over (ix src) unsafeTail board
  sccs = boardNumSCCs (not . null)

-- "Freedom to Move" rule: a piece cannot move to a cell that it cannot
-- physically slide to. This is implemented by checking that the destination
-- cell is unoccupied, and the number of neighbors shared between the source and
-- destination cells is less than two.
--
-- As an optimization, take the entire path, so the set of neighbors for each
-- cell only has to be calculated once.
--
-- Precondition: Path is all in bounds and adjacent, tail of path is all
-- unoccupied.
checkFreedomToMoveRule
  :: MonadError HiveError m
  => BoardIndex          -- ^ Path head
  -> NonEmpty BoardIndex -- ^ Path tail
  -> Board               -- ^ Board
  -> m ()
checkFreedomToMoveRule i0 is0 board =
  forM_ (adjacentPairs (i0 : NonEmpty.toList is0)) $ \(i,j) -> do
    when (cellIsOccupied board j) $
      throwError SlideToOccupiedCell

    checkPieceCanSlide
      (occupiedNeighborIndices i board)
      (occupiedNeighborIndices j board)
 where
  checkPieceCanSlide
    :: MonadError HiveError m
    => Set BoardIndex -- Source occupied neighbors
    -> Set BoardIndex -- Destination occupied neighbors
    -> m ()
  checkPieceCanSlide ns ms =
    when (length (Set.intersection ns ms) > 1) $
      throwError SlideToNonAdjacentCell

-- An ant slides around any number of cells.
validateAntMove
  :: MonadError HiveError m
  => BoardIndex
  -> NonEmpty BoardIndex
  -> Board
  -> m ()
validateAntMove i is board =
  checkFreedomToMoveRule i is board

-- A grasshopper hops over one or more occupied cells in a straight line
validateGrasshopperMove
  :: forall m.
     MonadError HiveError m
  => BoardIndex
  -> NonEmpty BoardIndex
  -> Board
  -> m ()
validateGrasshopperMove i is board = do
  when (length is == 1) $
    throwError GrasshopperMoveLength

  -- All cells along the path except for the last must be occupied
  when (any (cellIsUnoccupied board) inner) $
    throwError HopOverUnoccupiedCell

  -- The last cell must be unoccupied.
  when (cellIsOccupied board dst) $
    throwError HopToOccupiedCell

  -- Path forms a straight line, i.e. the adjacency relationship between
  -- consecutive cells is the same throughout. Note that this depends on
  -- the parity of the board, and is not as simple as taking the integer
  -- differences between the indices.
  checkFormsStraightLine (i : NonEmpty.toList is)
 where
  Just (inner, dst) = List.unsnoc (NonEmpty.toList is)

  -- Precondition: list has at least three elements.
  checkFormsStraightLine :: [BoardIndex] -> m ()
  checkFormsStraightLine (j0:j1:j2:js) = do
    unless (boardAdjacency board j0 j1 == boardAdjacency board j1 j2) $
      throwError HopInCrookedLine
    checkFormsStraightLine (j1:j2:js)
  checkFormsStraightLine _ = pure () -- should only be hit with 2 element list

-- A spider slides across three cells and cannot backtrack
validateSpiderMove
  :: MonadError HiveError m
  => BoardIndex
  -> NonEmpty BoardIndex
  -> Board
  -> m ()
validateSpiderMove i is board = do
  when (length is /= 3) $
    throwError SpiderMoveLength

  checkFreedomToMoveRule i is board

  unless (List.nub js == js) $
    throwError SpiderBacktrack
 where
  js = i : NonEmpty.toList is

-- A beetle moves to an adjacent tile.
validateBeetleMove
  :: MonadError HiveError m
  => NonEmpty BoardIndex
  -> m ()
validateBeetleMove is = do
  when (length is /= 1) $
    throwError BeetleMoveLength

-- A queen slides one cell.
validateQueenMove
  :: MonadError HiveError m
  => BoardIndex
  -> NonEmpty BoardIndex
  -> Board
  -> m ()
validateQueenMove i is board = do
  when (length is /= 1) $
    throwError QueenMoveLength

  checkFreedomToMoveRule i is board
