{-# LANGUAGE DeriveFunctor #-}

module Data.List.Zipper
    ( Z(..)
    , zfromList
    , ztoList
    , zpeek
    , zpoke
    , zleft
    , zleftUnsafe
    , zright
    , zrightUnsafe
    , module Control.Comonad
    ) where

import Control.Comonad
import Data.Maybe

data Z a = Z [a] a [a]
    deriving Functor

instance Comonad Z where
    extract = zpeek
    duplicate z = Z (iterateMaybe z zleft) z (iterateMaybe z zright)

zfromList :: [a] -> Maybe (Z a)
zfromList []     = Nothing
zfromList (x:xs) = Just (Z [] x xs)

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

zright :: Z a -> Maybe (Z a)
zright (Z _  _ [])     = Nothing
zright (Z as b (c:cs)) = Just (Z (b:as) c cs)

-- | Move right when you know you can.
zrightUnsafe :: Z a -> Z a
zrightUnsafe = fromJust . zright

-- Like iterate, but also drop the inital element.
iterateMaybe :: a -> (a -> Maybe a) -> [a]
iterateMaybe x f =
    case f x of
        Nothing -> []
        Just x' -> x' : iterateMaybe x' f
