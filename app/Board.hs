module Board where

import Types  

import Data.Array (array, (!), (//))
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Set (Set)
import Data.Char (isDigit, digitToInt)
import Data.List (intercalate, nub)

rows :: [Row]
rows = [1..9]

cols :: [Col]
cols = [1..9]

digits :: [Digit]
digits = [1..9]

positions :: [Pos]
positions = [(r,c) | r <- rows, c <- cols]

bounds9 :: (Pos, Pos)
bounds9 = ((1,1),(9,9))

allCands :: Set Digit
allCands = S.fromList digits

emptyBoard :: Board
emptyBoard = array bounds9 [ (p, Cands allCands) | p <- positions ]

-- Helpers to compute block coordinates
blockTopLeft :: Pos -> Pos
blockTopLeft (r,c) = (r0, c0)
  where
    r0 = ((r - 1) `div` 3) * 3 + 1
    c0 = ((c - 1) `div` 3) * 3 + 1

-- Positions in the same block as the given position
blockPositions :: Pos -> [Pos]
blockPositions p = [ (r + dr, c + dc) | dr <- [0..2], dc <- [0..2] ]
  where (r,c) = blockTopLeft p

-- Positions in the same row or column as the given position
rowPositions :: Pos -> [Pos]
rowPositions (r,_) = [(r,c) | c <- cols]

-- Positions in the same column as the given position
colPositions :: Pos -> [Pos]
colPositions (_,c) = [(r,c) | r <- rows]

-- Peers: all positions sharing row, col, or block, excluding the position itself
peersOf :: Pos -> Set Pos
peersOf p = S.delete p $ S.fromList (rowPositions p ++ colPositions p ++ blockPositions p)

-- Get and set cells
getCell :: Board -> Pos -> Cell
getCell b p = b ! p

setCell :: Board -> Pos -> Cell -> Board
setCell b p c = b // [(p,c)]

-- Parsing: accepts string with digits and dots/zeros for empties; ignores other whitespace
fromString :: String -> Either String Board
fromString s =
    let cs = filter (\r -> isDigit r || r `elem` ".0_") s
    in if length cs /= 81
        then Left ("Expected 81 cells, got " ++ show (length cs))
        else Right (array bounds9 (zip positions (map charToCell cs)))
    where
        charToCell ch
            | isDigit ch && ch /= '0' = Fixed (digitToInt ch)
            | otherwise = Cands allCands

-- Display board in a 9x9 grid with box dividers
displayBoard :: Board -> String
displayBoard b = intercalate "\n" $ addDividers (map showRow rows)
  where
    showRow r = intercalate " | " [group r c0 | c0 <- [1,4,7]]
    group r c0 = unwords [showCell (b ! (r,c)) | c <- [c0..c0+2]]
    showCell (Fixed d) = show d
    showCell (Cands _) = "."
    divider = "------+-------+------"
    addDividers rs = 
        let (top, rest) = splitAt 3 rs
            (mid, bot)  = splitAt 3 rest
        in top ++ [divider] ++ mid ++ [divider] ++ bot

-- Solved if every cell is Fixed
isSolved :: Board -> Bool
isSolved b = all isFixed (A.elems b)
    where
        isFixed (Fixed _) = True
        isFixed _ = False

-- Check if the board is valid: no duplicates in any row, column, or block
isValid :: Board -> Bool
isValid b = all unitValid allUnits
    where
        allUnits    = rowsUnits ++ colsUnits ++ blocksUnits
        rowsUnits   = [[(r,c) | c <- cols] | r <- rows]
        colsUnits   = [[(r,c) | r <- rows] | c <- cols]
        blocksUnits = [ [ (r0 + dr, c0 + dc) | dr <- [0..2], dc <- [0..2] ] | r0 <- [1,4,7], c0 <- [1,4,7] ]
        unitValid ps = 
            let fixed = [d | p <- ps, Fixed d <- [b ! p]]
            in length fixed == length (nub fixed)
