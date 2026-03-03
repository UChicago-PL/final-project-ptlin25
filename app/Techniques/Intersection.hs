module Techniques.Intersection where

import Types
import Board
import Candidates

import qualified Data.Set as S
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Control.Monad (msum)

boxUnits :: [[Pos]]
boxUnits = [[(r0+dr, c0+dc) | dr <- [0..2], dc <- [0..2]] | r0 <- [1,4,7], c0 <- [1,4,7]]

rowUnits :: [[Pos]]
rowUnits = [[(r,c) | c <- cols] | r <- rows]

colUnits :: [[Pos]]
colUnits = [[(r,c) | r <- rows] | c <- cols]

-- Pointing pairs/triples (box -> line):
-- If all candidates for a digit within a box lie in a single row or column,
-- eliminate that digit from the rest of that row or column outside the box.
pointingPairs :: Board -> Maybe Move
pointingPairs b = msum [ tryDigit d box | box <- boxUnits, d <- digits ]
  where
    -- If all candidates for digit d in the box lie in the same row, eliminate d from that row outside the box.
    -- If they lie in the same column, eliminate d from that column outside the box.
    tryDigit d box =
        let ps = [p | p <- box, d `S.member` getCands (getCell b p)]
        in if null ps then Nothing
           else case allSameRow ps of
                -- If all candidates for d in the box are in the same row, eliminate d from that row outside the box.
                Just r  -> listToMaybe [ Eliminate p d
                                      | p <- rowPositions (r, 1)  -- Get all positions in that row
                                      , p `notElem` box     -- Only eliminate if p is outside the box
                                      , d `S.member` getCands (getCell b p)     -- Only eliminate if d is still a candidate in that cell
                                      ]
                Nothing -> case allSameCol ps of
                    Just c  -> listToMaybe [ Eliminate p d
                                          | p <- colPositions (1, c) -- Get all positions in that column
                                          , p `notElem` box    -- Only eliminate if p is outside the box
                                          , d `S.member` getCands (getCell b p)    -- Only eliminate if d is still a candidate in that cell
                                          ]
                    Nothing -> Nothing

    -- Check if all positions are in the same row, returning that row if so.
    allSameRow ps = 
        let rs = nub [r | (r,_) <- ps]
        in if length rs == 1 then Just (head rs) else Nothing

    -- Check if all positions are in the same column, returning that column if so.
    allSameCol ps = 
        let cs = nub [c | (_,c) <- ps]
        in if length cs == 1 then Just (head cs) else Nothing

-- Box-line reduction (line -> box):
-- If all candidates for a digit within a row or column lie in a single box,
-- eliminate that digit from the rest of that box outside the row or column.
boxLineReduction :: Board -> Maybe Move
boxLineReduction b = msum [ tryDigit d unit | unit <- rowUnits ++ colUnits, d <- digits ]
  where
    -- If all candidates for digit d in the unit lie in the same box, eliminate d from that box outside the unit.
    tryDigit d unit =
        -- Positions in the unit that have d as a candidate
        let ps = [p | p <- unit, d `S.member` getCands (getCell b p)]
        in if null ps then Nothing
            -- All candidates for d in the unit are in the same box, so eliminate d from that box outside the unit.
           else case allSameBox ps of
               Just box -> listToMaybe [ Eliminate p d
                                       | p <- box
                                       , p `notElem` unit
                                       , d `S.member` getCands (getCell b p)
                                       ]
               Nothing  -> Nothing

    allSameBox []     = Nothing
    allSameBox (p:ps) = let tl = blockTopLeft p
                        in if all (\q -> blockTopLeft q == tl) ps
                           then Just (blockPositions p)
                           else Nothing
