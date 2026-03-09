module Main where

import Board (displayBoard)
import Solver (solveFromString)
import Generator (generatePuzzle)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--solve"]          -> interact (unlines . map solve . lines)
        ["--generate", seed] -> putStrLn (displayBoard (generatePuzzle (read seed)))
        _                    -> putStrLn "Usage: sudoku -- --solve | sudoku -- --generate <seed>"

solve :: String -> String
solve input =
    case solveFromString input of
        Left  err -> "Error: " ++ err
        Right b   -> "Solved\n" ++ displayBoard b ++ "\n"
