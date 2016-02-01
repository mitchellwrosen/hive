{-# LANGUAGE TemplateHaskell #-}

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
    , bzInsertRowAbove
    , bzInsertRowBelow
    , bzInsertColLeft
    , bzInsertColRight
    ) where

import Data.List.Zipper
import Hive.Types

import Control.Lens
import Control.Monad
import Data.List

-- | A board zipper that focuses on a single tile stack.
data BoardZipper = BZ
    { _bzabove :: ![Z TileStack] -- rows above
    , _bzcur   :: !(Z TileStack) -- current row
    , _bzbelow :: ![Z TileStack] -- rows below
    , _bzpar   :: !Parity        -- board parity
    , _bzw     :: !Int           -- width
    , _bzh     :: !Int           -- height
    , _bzrow   :: !Int           -- focused row, range: [0, height)
    , _bzcol   :: !Int           -- focused col, range: [0, width)
    }
makeLenses ''BoardZipper

-- Traversal on all rows of a board zipper.
bzrows :: Traversal' BoardZipper (Z TileStack)
bzrows f (BZ as b cs p w h row col) = (\as' b' cs' -> BZ as' b' cs' p w h row col)
    <$> traverse f as
    <*> f b
    <*> traverse f cs

instance Show BoardZipper where
    show (BZ as b cs _ _ _ _ _) =
        let
            as' = map ((" " ++) . show) (reverse as)
            b'  = show b
            cs' = map ((" " ++) . show) cs
        in
            intercalate "\n" (as' ++ [">" ++ b'] ++ cs')

-- | Create a BoardZipper from a Board.
bzFromBoard :: Board -> BoardZipper
bzFromBoard (Board (xs:xss) p w h) =
    BZ [] (zfromListUnsafe xs) (map zfromListUnsafe xss) p w h 0 0
bzFromBoard (Board [] _ _ _) = error "Invariant violated: empty board"

bzToBoard :: BoardZipper -> Board
bzToBoard (BZ as b cs p w h _ _) =
    let
        as' = reverse (map ztoList as)
        b'  = ztoList b
        cs' = map ztoList cs
    in
        Board (as' ++ [b'] ++ cs') p w h

bzPeek :: BoardZipper -> TileStack
bzPeek (BZ _ b _ _ _ _ _ _) = zpeek b

bzPoke :: TileStack -> BoardZipper -> BoardZipper
bzPoke tile = over bzcur (zpoke tile)

bzLeft :: BoardZipper -> Maybe BoardZipper
bzLeft (BZ as b cs p w h row col) =
    let
        -- Rely on laziness here - these will only be forced if the current row
        -- can safely move left, in which case the other rows can move left as
        -- well (assuming the Board is a rectangle).
        as' = map zleftUnsafe as
        cs' = map zleftUnsafe cs
    in
        (\b' -> BZ as' b' cs' p w h row (col-1)) <$> zleft b

bzRight :: BoardZipper -> Maybe BoardZipper
bzRight (BZ as b cs p w h row col) =
    let
        as' = map zrightUnsafe as
        cs' = map zrightUnsafe cs
    in
        (\b' -> BZ as' b' cs' p w h row (col+1)) <$> zright b

bzUp :: BoardZipper -> Maybe BoardZipper
bzUp (BZ [] _ _ _ _ _ _ _) = Nothing
bzUp (BZ (a:as) b cs p w h row col) = Just (BZ as a (b:cs) p w h (row-1) col)

bzDown :: BoardZipper -> Maybe BoardZipper
bzDown (BZ _ _ [] _ _ _ _ _) = Nothing
bzDown (BZ as b (c:cs) p w h row col) = Just (BZ (b:as) c cs p w h (row+1) col)

-- | Get all neighbors of a focused tile. Our neighbors depend on whether the
-- board has odd or even parity, as well as if we are focused on an odd-numbered
-- or even-numbered column.
bzNeighbors :: BoardZipper -> [TileStack]
bzNeighbors z@(BZ _ _ _ p _ _ _ col) = (fs <*> pure z)^..traverse._Just.to bzPeek
  where
    fs :: [BoardZipper -> Maybe BoardZipper]
    fs =
        case tileParity of
            Even ->
                [ bzUp
                , bzRight
                , bzDown >=> bzRight
                , bzDown
                , bzDown >=> bzLeft
                , bzLeft
                ]
            Odd ->
                [ bzUp
                , bzUp >=> bzRight
                , bzRight
                , bzDown
                , bzLeft
                , bzUp >=> bzLeft
                ]

    colParity :: Parity
    colParity
        | even col  = Even
        | otherwise = Odd

    tileParity :: Parity
    tileParity
        | p == colParity = Even
        | otherwise      = Odd

bzInsertRowAbove :: BoardZipper -> BoardZipper
bzInsertRowAbove bz =
    bz & bzabove %~ (newEmptyRow (bz^.bzw) (bz^.bzcol) :)
       & bzh     %~ (+1)
       & bzrow   %~ (+1)

bzInsertRowBelow :: BoardZipper -> BoardZipper
bzInsertRowBelow bz =
    bz & bzbelow %~ (newEmptyRow (bz^.bzw) (bz^.bzcol) :)
       & bzh     %~ (+1)

bzInsertColLeft :: BoardZipper -> BoardZipper
bzInsertColLeft bz =
    bz & bzrows %~ zinsertLeft []
       & bzw    %~ (+1)
       & bzcol  %~ (+1)
       & bzpar  %~ flipParity

bzInsertColRight :: BoardZipper -> BoardZipper
bzInsertColRight bz =
    bz & bzrows %~ zinsertRight []
       & bzw    %~ (+1)

newEmptyRow :: Int -> Int -> Z TileStack
newEmptyRow w x = zrightByUnsafe x (zfromListUnsafe (replicate w []))

flipParity :: Parity -> Parity
flipParity Even = Odd
flipParity Odd  = Even
