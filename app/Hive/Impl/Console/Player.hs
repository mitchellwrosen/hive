module Hive.Impl.Console.Player
    ( consolePlayer
    ) where

import Mitchell.Prelude

import Hive
import Hive.Impl.Common

import Control.Lens
import Prelude (String, read) -- TODO: Text
import System.Console.ANSI
import System.Console.Haskeline
import Text.Megaparsec

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text          as Text
import qualified Data.Vector        as Vector

consolePlayer :: String -> Game -> Hive (InputT IO) ()
consolePlayer name game0 = do
  liftIO $ do
    putStrLn (cs ("To place a piece: " ++ colored White "place ant 0 1"))
    putStrLn (cs ("To move a piece:  " ++ colored White "move 2 3, 2 4, 3 4"))
  loop game0
 where
  loop game = do
    liftIO $ do
      putStrLn ""
      printBoard (game^.gameBoard)

    let prompt = colored (player2color (game^.gamePlayer)) (name ++ "> ")
    lift (getInputLine prompt) >>= \case
      Nothing -> pure ()
      Just line ->
        case runParser actionParser "" line of
          Left _ -> do
            putStrLn (cs (colored Red "Parse error."))
            loop game
          Right action -> do
            result <-
              case action of
                Place bug idx -> makePlacement bug idx
                Move x xs     -> makeMove x xs

            case result of
              Left err -> do
                putStrLn (cs (colored Red ("Invalid move! " ++ Text.unpack err)))
                loop game
              Right (GameOver winner)  -> liftIO (print winner)
              Right (GameActive game') -> loop game'

actionParser :: Parsec String Action
actionParser = placeParser <|> moveParser
 where
  placeParser :: Parsec String Action
  placeParser = do
    _ <- string' "place"
    space
    bug <- bugParser
    space
    idx <- idxParser
    pure (Place bug idx)

  moveParser :: Parsec String Action
  moveParser = do
    _ <- string' "move"
    space
    (x:xs) <- sepBy1 idxParser (space *> char ',' <* space)
    pure (Move x (NonEmpty.fromList xs))

  bugParser :: Parsec String Bug
  bugParser =
        Ant         <$ string' "ant"
    <|> Grasshopper <$ string' "grasshopper"
    <|> Spider      <$ string' "spider"
    <|> Beetle      <$ string' "beetle"
    <|> Queen       <$ string' "queen"

  idxParser :: Parsec String BoardIndex
  idxParser = do
    row <- num
    space
    col <- num
    pure (row, col)
   where
    num = read <$> some digitChar

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn (cs (colored White column_nums))
  putStrLn (cs ("   " ++ replicate (3 * (board^.boardWidth) - 2) '-'))

  Vector.imapM_
    printTwoRows
    (Vector.map (split . Vector.toList . Vector.map cellToString) (board^.boardTiles))
 where
  column_nums :: String
  column_nums =
    "   " ++ concatMap (printf "%-2d ")
                       (take (board^.boardWidth) [(0::Int)..])

  printTwoRows :: Int -> ([String], [String]) -> IO ()
  printTwoRows row (es, os)
    | board^.boardParity == Even = do
        unless (null os) $
          putStrLn (cs (row_num ++ spaceFirst os))
        unless (null es) $
          putStrLn (cs (row_num ++ spaceSecond es))
    | otherwise = do
        unless (null es) $
          putStrLn (cs (row_num ++ spaceSecond es))
        unless (null os) $
          putStrLn (cs (row_num ++ spaceFirst os))
   where
    row_num = colored White (printf "%2d|" row)

  cellToString :: Cell -> String
  cellToString [] = "·"
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
