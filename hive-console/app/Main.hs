module Main where

import Mitchell.Prelude

import Hive                (Winner, runHive)
import Hive.Console.Player (completion, player)

import System.Console.Haskeline

main :: IO ()
main =
  runInputT (setComplete completion defaultSettings) game
    >>= print

game :: InputT IO (Maybe Winner)
game = runHive True True (player "Player 1") (player "Player 2")
