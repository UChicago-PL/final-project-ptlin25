module Solver where

import Types
import Board
import Candidates
import Techniques.Singles
import Techniques.Pairs
import Techniques.Intersection

import Control.Monad (msum)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as S

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

-- Pick the unsolved cell with fewest candidates for branching.
pickCell :: Board -> Maybe (Pos, S.Set Digit)
pickCell b =
    let unsolved = [(p, cs) | p <- positions, Cands cs <- [getCell b p]]
    in case unsolved of
        [] -> Nothing
        _  -> Just (minimumBy (comparing (S.size . snd)) unsolved)

-- Lazily enumerate all solutions via backtracking.
-- Uses constraint propagation (existing techniques) before branching.
solutions :: Board -> [Board]
solutions b
    | isSolved b = [b]
    | otherwise  = case solveStep b of
        Just (b', _) -> solutions b'
        Nothing      -> case pickCell b of
            Nothing      -> []
            Just (p, cs) -> concatMap branch (S.toList cs)
              where branch d = maybe [] solutions (assign b p d)

-- True iff the board has exactly one solution.
hasUniqueSolution :: Board -> Bool
hasUniqueSolution b = length (take 2 (solutions b)) == 1

-- Parse, initialize candidates, and solve.
solveFromString :: String -> Either String Board
solveFromString s = do
    b  <- fromString s
    b' <- maybe (Left "Contradiction during initialization") Right (initCands b)
    maybe (Left "Puzzle could not be solved with available techniques") Right (solve b')
