module Solver where

import Types
import Board
import Candidates
import Techniques.Singles
import Techniques.Pairs
import Techniques.Intersection

import Control.Monad (msum)

type Technique = Board -> Maybe Move

techniques :: [Technique]
techniques = [nakedSingle, hiddenSingle, pointingPairs, boxLineReduction, nakedPair, hiddenPair]

-- Try each technique in order; return the first Move found.
tryTechniques :: Board -> Maybe Move
tryTechniques b = msum (map ($ b) techniques)

-- Apply one technique, then re-propagate fixed-cell constraints.
solveStep :: Board -> Maybe (Board, Move)
solveStep b = do
    move <- tryTechniques b
    b'   <- applyMove b move
    return (b', move)

-- Repeatedly apply solveStep until solved or no progress can be made.
solve :: Board -> Maybe Board
solve b
    | isSolved b = Just b
    | otherwise  = case solveStep b of
        Nothing      -> Nothing
        Just (b', _) -> solve b'

-- Parse, initialize candidates, and solve.
solveFromString :: String -> Either String Board
solveFromString s = do
    b  <- fromString s
    b' <- maybe (Left "Contradiction during initialization") Right (initCands b)
    maybe (Left "Puzzle could not be solved with available techniques") Right (solve b')
