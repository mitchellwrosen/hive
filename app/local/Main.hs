{-# LANGUAGE LambdaCase #-}

module Main where

import Hive

import Control.Lens
import Control.Monad.IO.Class
import System.Console.ANSI

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector        as V

data Turn
    = Placement Bug BoardIndex
    | Move [BoardIndex]
    deriving Read

main :: IO ()
main = runHive (consolePlayer "Player 1") (consolePlayer "Player 2")

consolePlayer :: String -> Game -> Hive IO ()
consolePlayer name game = do
    liftIO $ do
        putStrLn (colored (player2color (game^.gamePlayer)) name ++ ", what's your move?")
        printBoard (game^.gameBoard)

    mturn <- reads <$> lift getLine
    case mturn of
        [(turn, "")] -> do
            result <-
                case turn of
                    Placement bug idx -> makePlacement bug idx
                    Move (x:xs) -> makeMove x (NE.fromList xs)
                    _ -> error "empty move"

            case result of
                Nothing -> do
                    liftIO (putStrLn "invalid move!")
                    consolePlayer name game
                Just (GameOver winner) -> liftIO (print winner)
                Just (GameActive game') -> consolePlayer name game'

        _ -> do
            liftIO (putStrLn "Parse error.")
            consolePlayer name game

printBoard :: Board -> IO ()
printBoard (HexBoard tiles parity) =
    mapM_ printTwoRows (V.map (split . V.toList . V.map cellToString) tiles)
  where
    printTwoRows :: ([String], [String]) -> IO ()
    printTwoRows (es, os)
        | parity == Even = do
            putStrLn (spaceFirst os)
            putStrLn (spaceSecond es)
        | otherwise = do
            putStrLn (spaceSecond es)
            putStrLn (spaceFirst os)

    cellToString :: Cell -> String
    cellToString [] = "-"
    cellToString (Tile p bug : _) = colored (player2color p) [bug2char bug]

bug2char :: Bug -> Char
bug2char Ant         = 'A'
bug2char Grasshopper = 'G'
bug2char Spider      = 'S'
bug2char Beetle      = 'B'
bug2char Queen       = 'Q'

player2color :: Player -> Color
player2color P1 = Magenta
player2color P2 = Cyan

colored :: Color -> String -> String
colored c s = setSGRCode [SetColor Foreground Vivid c] ++ s ++ setSGRCode [Reset]

spaceFirst :: [String] -> String
spaceFirst []  = ""
spaceFirst (s:ss) = "   " ++ s ++ spaceFirst' ss

spaceFirst' :: [String] -> String
spaceFirst' []  = ""
spaceFirst' (s:ss) = "     " ++ s ++ spaceFirst' ss

spaceSecond :: [String] -> String
spaceSecond [] = ""
spaceSecond (s:ss) = s ++ "     " ++ spaceSecond ss

split :: [a] -> ([a], [a])
split = go True ([], [])
  where
    go :: Bool -> ([a], [a]) -> [a] -> ([a], [a])
    go _ (es, os) [] = (reverse es, reverse os)
    go True (es, os) (x:xs) = go False (x:es, os) xs
    go False (es, os) (x:xs) = go True (es, x:os) xs
