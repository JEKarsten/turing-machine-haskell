module Main where

import Examples
import System.Environment

main :: IO ()
-- | takes the first command line argument and simulates the Turing machine in stdout
main =
    do
        arg:_ <- getArgs
        putStrLn (simulateFromString arg ++ "\n")