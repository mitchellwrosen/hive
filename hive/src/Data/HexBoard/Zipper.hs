{-# LANGUAGE TemplateHaskell #-}

-- | Hex board zipper. This module is intended to be imported qualified.
module Data.HexBoard.Zipper
    ( HexBoardZ
    , fromBoard
    , toBoard
    , peek
    , poke
    , left
    , right
    , up
    , down
    , neighbors
    , insertRowAbove
    , insertRowBelow
    , insertColLeft
    , insertColRight
    ) where

import Data.HexBoard
import Data.List.Zipper (Z)

import qualified Data.List.Zipper as Z

import Control.Lens
import Control.Monad
import Data.List

data HexBoardZ a = BZ
    { _bzabove :: ![Z a]  -- rows above
    , _bzcur   :: !(Z a)  -- current row
    , _bzbelow :: ![Z a]  -- rows below
    , _bzpar   :: !Parity -- board parity
    , _bzw     :: !Int    -- width
    , _bzh     :: !Int    -- height
    , _bzrow   :: !Int    -- focused row, range: [0, height)
    , _bzcol   :: !Int    -- focused col, range: [0, width)
    }
makeLenses ''HexBoardZ

-- Traversal on all rows of a board zipper.
bzrows :: Traversal' (HexBoardZ a) (Z a)
bzrows f (BZ as b cs p w h row col) = (\as' b' cs' -> BZ as' b' cs' p w h row col)
    <$> traverse f as
    <*> f b
    <*> traverse f cs

instance Show a => Show (HexBoardZ a) where
    show (BZ as b cs _ _ _ _ _) =
        let
            as' = map ((" " ++) . show) (reverse as)
            b'  = show b
            cs' = map ((" " ++) . show) cs
        in
            intercalate "\n" (as' ++ [">" ++ b'] ++ cs')

fromBoard :: HexBoard a -> Maybe (HexBoardZ a)
fromBoard (HexBoard [] _ _ _) = Nothing
fromBoard (HexBoard (xs:xss) p w h) =
    Just (BZ [] (Z.fromListUnsafe xs) (map Z.fromListUnsafe xss) p w h 0 0)

fromBoardUnsafe :: HexBoard a -> HexBoardZ a
fromBoardUnsafe board =
    case fromBoard board of
        Nothing -> error "fromBoardUnsafe: empty board"
        Just z  -> z

toBoard :: HexBoardZ a -> HexBoard a
toBoard (BZ as b cs p w h _ _) =
    let
        as' = reverse (map Z.toList as)
        b'  = Z.toList b
        cs' = map Z.toList cs
    in
        HexBoard (as' ++ [b'] ++ cs') p w h

peek :: HexBoardZ a -> a
peek = view (bzcur.to Z.peek)

poke :: a -> HexBoardZ a -> HexBoardZ a
poke tile = over bzcur (Z.poke tile)

left :: HexBoardZ a -> Maybe (HexBoardZ a)
left z = do
    cur' <- Z.left (z^.bzcur)
    -- If left on the current row is safe, then left on the other rows is safe
    pure (z & bzabove.traverse %~ Z.leftUnsafe
            & bzcur            .~ cur'
            & bzbelow.traverse %~ Z.leftUnsafe
            & bzcol            %~ subtract 1)

right :: HexBoardZ a -> Maybe (HexBoardZ a)
right z = do
    cur' <- Z.right (z^.bzcur)
    -- If right on the current row is safe, then right on the other rows is safe
    pure (z & bzabove.traverse %~ Z.rightUnsafe
            & bzcur            .~ cur'
            & bzbelow.traverse %~ Z.rightUnsafe
            & bzcol            %~ (+1))

up :: HexBoardZ a -> Maybe (HexBoardZ a)
up (BZ [] _ _ _ _ _ _ _) = Nothing
up (BZ (a:as) b cs p w h row col) = Just (BZ as a (b:cs) p w h (row-1) col)

down :: HexBoardZ a -> Maybe (HexBoardZ a)
down (BZ _ _ [] _ _ _ _ _) = Nothing
down (BZ as b (c:cs) p w h row col) = Just (BZ (b:as) c cs p w h (row+1) col)

-- | Get all neighbors of a focused tile. Our neighbors depend on whether the
-- board has odd or even parity, as well as if we are focused on an odd-numbered
-- or even-numbered column.
neighbors :: HexBoardZ a -> [a]
neighbors z@(BZ _ _ _ p _ _ _ col) = (fs <*> pure z)^..traverse._Just.to peek
  where
    fs :: [HexBoardZ a -> Maybe (HexBoardZ a)]
    fs =
        case tileParity of
            Even ->
                [ up
                , right
                , down >=> right
                , down
                , down >=> left
                , left
                ]
            Odd ->
                [ up
                , up >=> right
                , right
                , down
                , left
                , up >=> left
                ]

    colParity :: Parity
    colParity
        | even col  = Even
        | otherwise = Odd

    tileParity :: Parity
    tileParity
        | p == colParity = Even
        | otherwise      = Odd

insertRowAbove :: a -> HexBoardZ a -> HexBoardZ a
insertRowAbove el z =
    z & bzabove %~ (newEmptyRow (z^.bzw) (z^.bzcol) el :)
       & bzh    %~ (+1)
       & bzrow  %~ (+1)

insertRowBelow :: a -> HexBoardZ a -> HexBoardZ a
insertRowBelow el z =
    z & bzbelow %~ (newEmptyRow (z^.bzw) (z^.bzcol) el :)
      & bzh     %~ (+1)

newEmptyRow :: Int -> Int -> a -> Z a
newEmptyRow w col el = Z.rightByUnsafe col (Z.fromListUnsafe (replicate w el))

insertColLeft :: a -> HexBoardZ a -> HexBoardZ a
insertColLeft el z =
    z & bzrows %~ Z.insertLeft el
      & bzw    %~ (+1)
      & bzcol  %~ (+1)
      & bzpar  %~ flipParity

insertColRight :: a -> HexBoardZ a -> HexBoardZ a
insertColRight el z =
    z & bzrows %~ Z.insertRight el
      & bzw    %~ (+1)
