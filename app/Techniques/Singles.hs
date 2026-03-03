module Techniques.Singles where

import Types
import Board
import Candidates

import qualified Data.Array as A
import qualified Data.Set as S
import Data.List (find)
import Control.Applicative ((<|>))

-- Naked single: a cell with exactly one candidate
nakedSingle :: Board -> Maybe Move
nakedSingle b = do
    (p, cell) <- find (isNaked . snd) (A.assocs b)
    d <- onlyCand cell
    return $ Place p d

-- Hidden single: a digit that can only go in one cell within a unit
hiddenSingle :: Board -> Maybe Move
hiddenSingle b = foldr (\unit acc -> findHidden unit <|> acc) Nothing allUnits
  where
    allUnits = rowUnits ++ colUnits ++ boxUnits
    rowUnits = [[(r,c) | c <- cols] | r <- rows]
    colUnits = [[(r,c) | r <- rows] | c <- cols]
    boxUnits = [[(r0+dr, c0+dc) | dr <- [0..2], dc <- [0..2]] | r0 <- [1,4,7], c0 <- [1,4,7]]
    
    findHidden unit = foldr (\d acc -> tryDigit d unit <|> acc) Nothing digits
    
    tryDigit d unit =
        case [p | p <- unit, d `S.member` getCands (getCell b p)] of
            [p] -> Just (Place p d)
            _   -> Nothing
