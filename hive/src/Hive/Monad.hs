module Hive.Monad
  ( Hive
  , pattern MakePlacement
  , pattern MakeMove
  , hiveAction
  , runHive
  ) where

import Mitchell.Prelude

import Hive.Action
import Hive.Error
import Hive.Expansions
import Hive.Game
import Hive.Player

import Control.Monad.Trans.Free
import Prelude (fmap)


-- | Underlying player action functor.
data HiveF a
  = HiveAction Action (Either HiveError GameState -> a)

instance Functor HiveF where
  fmap f (HiveAction act k) = HiveAction act (f . k)

pattern MakePlacement bug idx k = HiveAction (Place bug idx) k
pattern MakeMove i is k = HiveAction (Move i is) k


-- The Hive monad, where player actions and inner monad actions are
-- interleaved.
type Hive m a = FreeT HiveF m a


hiveAction :: Monad m => Action -> Hive m (Either HiveError GameState)
hiveAction x = liftF (HiveAction x identity)


-- | Run a two-player Hive game.
runHive
  :: Monad m
  => UseLadybug
  -> UseMosquito
  -> (Game -> Hive m ()) -- Player 1
  -> (Game -> Hive m ()) -- Player 2
  -> m (Maybe Winner)    -- Nothing if some player exited early
runHive ladybug mosquito p1 p2 =
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
  game = initialGame ladybug mosquito

runHive'
  :: forall m.
     Monad m
  => Game                     -- Game board
  -> Hive m ()                -- Current player logic
  -> (GameState -> Hive m ()) -- Next player logic, waiting for board resulting
                              --   from current player's move
  -> m (Maybe Winner)
runHive' game cur_player next_player =
  runFreeT cur_player >>= \case
    Pure () -> pure Nothing
    Free (MakePlacement bug idx k) -> go (Place bug idx) k
    Free (MakeMove i is k)         -> go (Move i is) k
 where
  go :: Action -> (Either HiveError GameState -> Hive m ()) -> m (Maybe Winner)
  go act k =
    case stepGame act game of
      Left err -> runHive' game (k (Left err)) next_player
      Right game_state ->
        case game_state of
          GameActive game' ->
            runHive' game' (next_player game_state) (k . Right)
          GameOver winner -> do
            stepPlayer (k (Right game_state))
            stepPlayer (next_player game_state)
            pure (Just winner)

-- | Step a player action once, to cause side-effects. This is useful for
-- informing a player that the game has ended, because we don't care about
-- what the player's followup actions might be (in fact, anything besides
-- 'Pure' indicates the player didn't properly respond to the 'GameOver'.
stepPlayer :: Monad m => Hive m () -> m ()
stepPlayer = void . runFreeT
