module Types where

import Data.Array (Array)
import Data.Set (Set)

type Row = Int
type Col = Int
type Digit = Int

type Pos = (Row, Col)

data Cell
    = Fixed Digit
    | Cands (Set Digit)
    deriving (Eq, Ord)

type Board = Array Pos Cell

data Move
    = Place Pos Digit
    | Eliminate Pos Digit
    deriving (Show)
