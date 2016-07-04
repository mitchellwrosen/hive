module Utils where

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs [_] = []
adjacentPairs (x:y:ys) = (x,y) : adjacentPairs (y:ys)
