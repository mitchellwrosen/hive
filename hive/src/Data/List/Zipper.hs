{-# LANGUAGE DeriveFunctor #-}

module Data.List.Zipper
    ( Z(..)
    , zfromList
    , zfromListUnsafe
    , ztoList
    , zpeek
    , zpoke
    , zleft
    , zleftUnsafe
    , zleftBy
    , zleftByUnsafe
    , zright
    , zrightUnsafe
    , zrightBy
    , zrightByUnsafe
    , zinsertLeft
    , zinsertRight
    , module Control.Comonad
    ) where

import Control.Comonad
import Control.Monad
import Data.List
import Data.Maybe

data Z a = Z [a] a [a]
    deriving Functor

instance Show a => Show (Z a) where
    show (Z as b cs) =
        let
            as' = map show (reverse as)
            b'  = ">" ++ show b ++ "<"
            cs' = map show cs
        in
            "[" ++ intercalate "," (as' ++ [b'] ++ cs') ++ "]"

instance Comonad Z where
    extract = zpeek
    duplicate z = Z (iterateMaybe z zleft) z (iterateMaybe z zright)

zfromList :: [a] -> Maybe (Z a)
zfromList []     = Nothing
zfromList (x:xs) = Just (Z [] x xs)

zfromListUnsafe :: [a] -> Z a
zfromListUnsafe = fromJust . zfromList

ztoList :: Z a -> [a]
ztoList (Z as b cs) = as ++ [b] ++ cs

zpeek :: Z a -> a
zpeek (Z _ x _) = x

zpoke :: a -> Z a -> Z a
zpoke b (Z as _ cs) = Z as b cs

zleft :: Z a -> Maybe (Z a)
zleft (Z []     _ _)  = Nothing
zleft (Z (a:as) b cs) = Just (Z as a (b:cs))

-- | Move left when you know you can.
zleftUnsafe :: Z a -> Z a
zleftUnsafe = fromJust . zleft

zleftBy :: Int -> Z a -> Maybe (Z a)
zleftBy 0 = pure
zleftBy n = zleft >=> zleftBy (n-1)

zleftByUnsafe :: Int -> Z a -> Z a
zleftByUnsafe n = fromJust . zleftBy n

zright :: Z a -> Maybe (Z a)
zright (Z _  _ [])     = Nothing
zright (Z as b (c:cs)) = Just (Z (b:as) c cs)

-- | Move right when you know you can.
zrightUnsafe :: Z a -> Z a
zrightUnsafe = fromJust . zright

zrightBy :: Int -> Z a -> Maybe (Z a)
zrightBy 0 = pure
zrightBy n = zright >=> zrightBy (n-1)

zrightByUnsafe :: Int -> Z a -> Z a
zrightByUnsafe n = fromJust . zrightBy n

zinsertLeft :: a -> Z a -> Z a
zinsertLeft a (Z as b cs) = Z (a:as) b cs

zinsertRight :: a -> Z a -> Z a
zinsertRight c (Z as b cs) = Z as b (c:cs)

-- Like iterate, but also drop the inital element.
iterateMaybe :: a -> (a -> Maybe a) -> [a]
iterateMaybe x f =
    case f x of
        Nothing -> []
        Just x' -> x' : iterateMaybe x' f
