module Generator where

import Types
import Board
import Candidates
import Solver
import Control.Applicative
import Data.Maybe (fromJust)

import qualified Data.Set as S
import qualified Data.Array as A
import System.Random (StdGen, mkStdGen, randomR, SplitGen (splitGen))

-- Fisher-Yates shuffle.
shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle g []  = ([], g)
shuffle g [x] = ([x], g)
shuffle g xs  =
    let n       = length xs
        (i, g') = randomR (0, n - 1) g
    in case splitAt i xs of
        (pre, x:post) -> let (rest, g'') = shuffle g' (pre ++ post)
                         in (x : rest, g'')
        _             -> (xs, g)  -- unreachable: i is always in [0, n-1]

-- Generate a random fully solved board by backtracking with shuffled candidates.
randomSolvedBoard :: StdGen -> Maybe Board
randomSolvedBoard gen = go gen emptyBoard
  where
    go g b
        | isSolved b = Just b
        | otherwise  = case solveStep b of
            Just (b', _) -> go g b'
            Nothing      -> case pickCell b of
                Nothing      -> Nothing
                Just (p, cs) ->
                    let (ds, g') = shuffle g (S.toList cs)
                    in foldr (tryDigit g') Nothing ds
                  where
                    tryDigit g' d acc = case assign b p d of
                        Nothing -> acc
                        Just b' -> go g' b' <|> acc

-- Build a board with only the given positions fixed; everything else empty.
buildBoard :: S.Set Pos -> Board -> Board
buildBoard givens sol =
    A.array bounds9 [ (p, if p `S.member` givens then sol A.! p else Cands allCands)
                    | p <- positions ]

-- Starting from a solved board, iteratively remove clues while the puzzle
-- retains a unique solution findable by the solver.
removeClues :: StdGen -> Board -> Board
removeClues g sol =
    let (shuffled, _) = shuffle g positions
        givens        = foldl tryRemove (S.fromList positions) shuffled
    in buildBoard givens sol
  where
    tryRemove givens p =
        let givens' = S.delete p givens
        in case initCands (buildBoard givens' sol) of
            Nothing -> givens
            Just b' -> if hasUniqueSolution b' then givens' else givens

-- Generate a puzzle from an integer seed.
-- randomSolvedBoard always succeeds on an empty board, so fromJust is safe.
generatePuzzle :: Int -> Board
generatePuzzle seed =
    let (g1, g2) = splitGen (mkStdGen seed)
        sol      = fromJust (randomSolvedBoard g1)
    in removeClues g2 sol
