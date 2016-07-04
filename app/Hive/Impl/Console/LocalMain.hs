module Main where

import Mitchell.Prelude

import Hive
import Hive.Impl.Console.Player

import System.Console.Haskeline

main :: IO ()
main =
  runInputT (setComplete consoleCompletion defaultSettings) game
    >>= print

game :: InputT IO (Maybe Winner)
game = runHive (consolePlayer "Player 1") (consolePlayer "Player 2")
