module Techniques.Pairs where

import Types
import Board
import Candidates

import qualified Data.Set as S
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Control.Monad (msum)

allUnits :: [[Pos]]
allUnits = rowUnits ++ colUnits ++ boxUnits
  where
    rowUnits = [[(r,c) | c <- cols] | r <- rows]
    colUnits = [[(r,c) | r <- rows] | c <- cols]
    boxUnits = [[(r0+dr, c0+dc) | dr <- [0..2], dc <- [0..2]] | r0 <- [1,4,7], c0 <- [1,4,7]]

-- All 2-element combinations from a list
choose2 :: [a] -> [(a, a)]
choose2 xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

-- Naked pair: two cells in a unit whose combined candidates are exactly 2 digits.
-- Eliminate those 2 digits from all other cells in the unit.
nakedPair :: Board -> Maybe Move
nakedPair b = msum
    [ listToMaybe (eliminations unit p1 p2 combined)
    | unit <- allUnits
    , ((p1,cs1),(p2,cs2)) <- choose2 (candCells unit)
    , let combined = cs1 `S.union` cs2
    , S.size combined == 2
    ]
  where
    candCells unit    = [(p, cs) | p <- unit, Cands cs <- [getCell b p]]
    eliminations unit p1 p2 combined =
        [ Eliminate p d
        | p <- unit, p /= p1, p /= p2
        , Cands cs <- [getCell b p]
        , d <- S.toList (combined `S.intersection` cs)
        ]

-- Hidden pair: two digits restricted to exactly two cells in a unit.
-- Eliminate all other candidates from those two cells.
hiddenPair :: Board -> Maybe Move
hiddenPair b = msum
    [ listToMaybe (eliminations ps1 d1 d2)
    | unit <- allUnits
    , (d1, d2) <- choose2 digits
    , let ps1 = candPositions d1 unit
    , let ps2 = candPositions d2 unit
    , length ps1 == 2 && ps1 == ps2
    ]
  where
    candPositions d unit = [p | p <- unit, d `S.member` getCands (getCell b p)]
    eliminations ps d1 d2 =
        [ Eliminate p d
        | p <- ps
        , Cands cs <- [getCell b p]
        , d <- S.toList (cs `S.difference` S.fromList [d1, d2])
        ]
