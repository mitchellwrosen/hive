module Hive.BoardZipper
    ( BoardZipper
    , bzFromBoard
    , bzToBoard
    , bzPeek
    , bzPoke
    , bzLeft
    , bzRight
    , bzUp
    , bzDown
    , bzNeighbors
    ) where

import Data.List.Zipper
import Hive.Types

import Control.Lens
import Control.Monad
import Data.Maybe

-- Even or odd numbered column
data Col
    = Even
    | Odd

flipCol :: Col -> Col
flipCol Even = Odd
flipCol Odd  = Even

-- | A board zipper that focuses on a single tile stack, and keeps track of the
-- parity of the currently focused row's currently focused column. This matters
-- because depending on if we're on an even or odd numbered column, our
-- neighbors change.
--
-- See the "odd-q" vertical layout here for more details:
-- http://www.redblobgames.com/grids/hexagons/#coordinates
data BoardZipper
    = BZ [Z TileStack] (Z TileStack) Col [Z TileStack]

-- | Create a BoardZipper from a Board. Fails if there are no tiles on the
-- board, succeeds otherwise.
bzFromBoard :: Board -> Maybe BoardZipper
bzFromBoard (Board []) = Nothing
bzFromBoard (Board (xs:xss)) =
    let
        -- xs must be non-empty
        z = fromJust (zfromList xs)
        -- xss each contain the same number of elements as xs, but we
        -- don't bother checking that here
        zs = map (fromJust . zfromList) xss
    in
        Just (BZ [] z Even zs)

bzToBoard :: BoardZipper -> Board
bzToBoard (BZ as b _ cs) =
    Board (map ztoList as ++ [ztoList b] ++ map ztoList cs)

bzPeek :: BoardZipper -> TileStack
bzPeek (BZ _ z _ _) = zpeek z

bzPoke :: TileStack -> BoardZipper -> BoardZipper
bzPoke tile (BZ as b col cs) = BZ as (zpoke tile b) col cs

bzLeft :: BoardZipper -> Maybe BoardZipper
bzLeft (BZ z0 z1 col z2) =
    (\z1' -> BZ (map zleftUnsafe z0) z1' (flipCol col) (map zleftUnsafe z2))
        <$> zleft z1

bzRight :: BoardZipper -> Maybe BoardZipper
bzRight (BZ z0 z1 col z2) =
    (\z1' -> BZ (map zrightUnsafe z0) z1' (flipCol col) (map zrightUnsafe z2))
        <$> zright z1

bzUp :: BoardZipper -> Maybe BoardZipper
bzUp (BZ []     _ _   _)  = Nothing
bzUp (BZ (a:as) b col cs) = Just (BZ as a col (b:cs))

bzDown :: BoardZipper -> Maybe BoardZipper
bzDown (BZ _  _ _   [])     = Nothing
bzDown (BZ as b col (c:cs)) = Just (BZ (b:as) c col cs)

-- | Get all neighbors of a focused tile.
-- See the "odd-q" vertical layout here for details:
-- http://www.redblobgames.com/grids/hexagons/#coordinates
bzNeighbors :: BoardZipper -> [TileStack]
bzNeighbors z@(BZ _ _ Even _) = bzNeighbors' z
    [ bzUp
    , bzUp >=> bzRight
    , bzRight
    , bzDown
    , bzLeft
    , bzUp >=> bzLeft
    ]
bzNeighbors z@(BZ _ _ Odd _) = bzNeighbors' z
    [ bzUp
    , bzRight
    , bzDown >=> bzRight
    , bzDown
    , bzDown >=> bzLeft
    , bzLeft
    ]

bzNeighbors' :: BoardZipper -> [BoardZipper -> Maybe BoardZipper] -> [TileStack]
bzNeighbors' z fs = (fs <*> pure z)^..traverse._Just.to bzPeek
