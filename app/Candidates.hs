module Candidates where

import Types
import Board

import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad (foldM)

-- Extract candidates from a cell (empty set if Fixed)
getCands :: Cell -> Set Digit
getCands (Cands cs) = cs
getCands (Fixed _)  = S.empty

-- True if a cell has exactly one candidate
isNaked :: Cell -> Bool
isNaked (Cands cs) = S.size cs == 1
isNaked (Fixed _)  = False

-- Extract the single digit from a size-1 Cands cell
onlyCand :: Cell -> Maybe Digit
onlyCand (Cands cs)
    | S.size cs == 1 = Just (S.findMin cs)
    | otherwise      = Nothing
onlyCand (Fixed _)   = Nothing

-- Remove digit d from candidates at p. Nothing on contradiction.
eliminate :: Board -> Pos -> Digit -> Maybe Board
eliminate b p d =
    case getCell b p of
        Fixed _  -> Just b
        Cands cs ->
            let cs' = S.delete d cs
            in if S.null cs'
                then Nothing
                else Just (setCell b p (Cands cs'))

-- Fix position p to digit d, eliminating d from all peers.
assign :: Board -> Pos -> Digit -> Maybe Board
assign b p d =
    let b' = setCell b p (Fixed d)
    in foldM (\acc peer -> eliminate acc peer d) b' (peersOf p)

-- Propagate all clues from a freshly parsed board.
initCands :: Board -> Maybe Board
initCands b = foldM applyFixed b positions
  where
    applyFixed acc p =
        case getCell acc p of
            Fixed d -> assign acc p d
            Cands _ -> Just acc

-- Apply a Move to the board.
applyMove :: Board -> Move -> Maybe Board
applyMove b (Place p d)     = assign b p d
applyMove b (Eliminate p d) = eliminate b p d
