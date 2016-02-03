module Main where

import Hive
import Lib

import System.Console.Haskeline

main :: IO ()
main =
    runInputT defaultSettings
        (runHive (consolePlayer "Player 1")
                 (consolePlayer "Player 2"))
