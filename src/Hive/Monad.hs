module Hive.Monad
  ( Hive
  , Action(..)
  , pattern MakePlacement
  , pattern MakeMove
  , hiveAction
  ) where

import Mitchell.Prelude

import Data.HexBoard (BoardIndex)
import Hive.Bug
import Hive.Game

import Data.Aeson
import Control.Monad.Trans.Free
import Prelude                  (fail)

import qualified Data.List.NonEmpty as NonEmpty

-- | A turn action. Either place a new bug or move an existing one.
data Action
  = Place Bug BoardIndex
  | Move BoardIndex (NonEmpty BoardIndex)
  deriving (Eq, Show)

instance ToJSON Action where
  toJSON = \case
    Place bug idx -> object
      [ "type"   .= String "Place"
      , "bug"    .= toJSON bug
      , "index"  .= toJSON idx
      ]
    Move idx idxs -> object
      [ "type" .= String "Move"
      , "path" .= toJSON (idx : NonEmpty.toList idxs)
      ]

instance FromJSON Action where
  parseJSON = withObject "object" $ \o ->
    o .: "type" >>=
      withText "text" (\case
        "Place" -> do
          bug <- o .: "bug"
          idx <- o .: "index"
          pure (Place bug idx)

        "Move" ->
          o .: "path" >>= \case
            (x:y:z) -> pure (Move x (NonEmpty.fromList (y:z)))
            _ -> fail "expected 2 or more board indices"

        _ -> fail "expected type Place or Move")


-- | Underlying player action functor.
data HiveF a
  = HiveAction Action (Either Text GameState -> a)
  deriving Functor

pattern MakePlacement bug idx k = HiveAction (Place bug idx) k
pattern MakeMove i is k = HiveAction (Move i is) k


-- The Hive monad, where player actions and inner monad actions are
-- interleaved.
type Hive m a = FreeT HiveF m a


hiveAction :: Monad m => Action -> Hive m (Either Text GameState)
hiveAction x = liftF (HiveAction x identity)
