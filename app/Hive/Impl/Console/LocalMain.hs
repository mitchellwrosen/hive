module Main where

import Hive
import Hive.Impl.Console.Player (consolePlayer)

import System.Console.Haskeline

main :: IO ()
main =
    runInputT defaultSettings
        (runHive (consolePlayer "Player 1")
                 (consolePlayer "Player 2"))
