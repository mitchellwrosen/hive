{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hive
    ( Hive
    , makeFirstPlacement
    , makePlacement
    , makeMove
    , runHive
    ) where

import Data.List.Zipper
import Hive.Types

import Control.Lens
import Control.Monad.Trans.Free

import qualified Data.List as List

--------------------------------------------------------------------------------
-- Hive monad

-- | Underlying Hive action functor.
data HiveF a
    -- Place the very first tile. During interpretation, the continuation is
    -- fed Nothing if the move was invalid, and Just otherwise. The same is
    -- true for the continuations below.
    = MakeFirstPlacement Bug (Maybe TurnResult -> a)
    -- Place a new tile.
    | MakePlacement Placement (Maybe TurnResult -> a)
    -- Move an already-placed tile.
    | MakeMove Move (Maybe TurnResult -> a)
    deriving Functor

-- The Hive monad, where Hive actions and inner monad actions are interleaved.
type Hive m a = FreeT HiveF m a

--------------------------------------------------------------------------------
-- Hive DSL

makeFirstPlacement :: Monad m => Bug -> Hive m (Maybe TurnResult)
makeFirstPlacement tile = liftF (MakeFirstPlacement tile id)

makePlacement :: Monad m => Placement -> Hive m (Maybe TurnResult)
makePlacement placement = liftF (MakePlacement placement id)

makeMove :: Monad m => Move -> Hive m (Maybe TurnResult)
makeMove move = liftF (MakeMove move id)

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
            GameCont game' -> p2 game')
  where
    game :: Game
    game = Game [[[]]] P1 bugs bugs

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
    => Game                      -- Game board
    -> Hive m ()                 -- Current player logic
    -> (TurnResult -> Hive m ()) -- Next player logic, waiting for board resulting
                                 --   from current player's move
    -> m ()
runHive' game cur_player next_player =
    runFreeT cur_player >>= \case
        -- The current player's logic has ended, so we have nothing left to
        -- interpret.
        Pure () -> pure ()

        -- If the board has any tiles on it, this fails. If it doesn't, we can
        -- assume that the current player is P1.
        Free (MakeFirstPlacement bug k) ->
            if nullOf boardTiles (game^.gameBoard)
                then
                    let
                        -- Updated game state.
                        game' :: Game
                        game' = game
                            & gameBoard  .~ [[[Tile (game^.gamePlayer) bug]]]
                            & gamePlayer %~ nextPlayer
                            & gameP1Bugs %~ List.delete bug

                        -- Current player's continuation, which receives the
                        -- result of the next player's move.
                        cur_player' :: TurnResult -> Hive m ()
                        cur_player' res = k (Just res)

                        -- Next player logic, given the result of the current
                        -- player taking his/her turn.
                        --
                        -- This is what feeds the partial pattern match from
                        -- 'runHive', where we assume the result of the first
                        -- tile being placed is a GameCont (which it is).
                        next_player' :: Hive m ()
                        next_player' = next_player (GameCont game')
                    in
                        runHive' game' next_player' cur_player'

                else runHive' game (k Nothing) next_player

        Free (MakePlacement (Placement tile (x,y) adj) k) ->
            error "TODO"

        Free (MakeMove (Move path adj) k) ->
            error "TODO"
  where
    -- Lens onto the current player's unplaced bugs.
    gameCurPlayerBugs :: Lens' Game [Bug]
    gameCurPlayerBugs =
        case game^.gamePlayer of
            P1 -> gameP1Bugs
            P2 -> gameP2Bugs

-- | Get the winner of this board, if any.
boardWinner :: Board -> Maybe Winner
boardWinner _ = Nothing

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1
