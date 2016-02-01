{-# LANGUAGE DeriveFunctor #-}

-- | List zipper. This module is intended to be imported qualified.
module Data.List.Zipper
    ( Z(..)
    , fromList
    , fromListUnsafe
    , toList
    , peek
    , poke
    , left
    , leftUnsafe
    , leftBy
    , leftByUnsafe
    , right
    , rightUnsafe
    , rightBy
    , rightByUnsafe
    , insertLeft
    , insertRight
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
    extract = peek
    duplicate z = Z (iterateMaybe z left) z (iterateMaybe z right)

fromList :: [a] -> Maybe (Z a)
fromList []     = Nothing
fromList (x:xs) = Just (Z [] x xs)

fromListUnsafe :: [a] -> Z a
fromListUnsafe xs =
    case fromList xs of
        Nothing -> error "fromListUnsafe: empty list"
        Just z  -> z

toList :: Z a -> [a]
toList (Z as b cs) = as ++ [b] ++ cs

peek :: Z a -> a
peek (Z _ x _) = x

poke :: a -> Z a -> Z a
poke b (Z as _ cs) = Z as b cs

left :: Z a -> Maybe (Z a)
left (Z []     _ _)  = Nothing
left (Z (a:as) b cs) = Just (Z as a (b:cs))

leftUnsafe :: Z a -> Z a
leftUnsafe z =
    case left z of
        Nothing -> error "leftUnsafe: no left element"
        Just z' -> z'

leftBy :: Int -> Z a -> Maybe (Z a)
leftBy 0 = pure
leftBy n = left >=> leftBy (n-1)

leftByUnsafe :: Int -> Z a -> Z a
leftByUnsafe n z =
    case leftBy n z of
        Nothing -> error ("leftByUnsafe: not " ++ show n ++ " left elements")
        Just z' -> z'

right :: Z a -> Maybe (Z a)
right (Z _  _ [])     = Nothing
right (Z as b (c:cs)) = Just (Z (b:as) c cs)

rightUnsafe :: Z a -> Z a
rightUnsafe z =
    case right z of
        Nothing -> error "rightUnsafe: no right element"
        Just z' -> z'

rightBy :: Int -> Z a -> Maybe (Z a)
rightBy 0 = pure
rightBy n = right >=> rightBy (n-1)

rightByUnsafe :: Int -> Z a -> Z a
rightByUnsafe n z =
    case rightBy n z of
        Nothing -> error ("rightByUnsafe: not " ++ show n ++ " right elements")
        Just z' -> z'


insertLeft :: a -> Z a -> Z a
insertLeft a (Z as b cs) = Z (a:as) b cs

insertRight :: a -> Z a -> Z a
insertRight c (Z as b cs) = Z as b (c:cs)

-- Like iterate, but also drop the inital element.
iterateMaybe :: a -> (a -> Maybe a) -> [a]
iterateMaybe x f =
    case f x of
        Nothing -> []
        Just x' -> x' : iterateMaybe x' f
