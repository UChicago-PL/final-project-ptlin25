module Main where

import Board (displayBoard)
import Solver (solveFromString)

main :: IO ()
main = interact (unlines . map run . lines)

run :: String -> String
run input =
    case solveFromString input of
        Left  err -> "Error: " ++ err
        Right b   -> "Solved\n" ++ displayBoard b ++ "\n"
