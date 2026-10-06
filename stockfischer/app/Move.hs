module Move where

import Data.Char

data Position = Position Int Int
  deriving (Eq, Ord)
data Move = Move Position Position

instance Read Position where
  readsPrec _ [file, rank] = do
    col <- case toUpper file of
      'A' -> [1]
      'B' -> [2]
      'C' -> [3]
      'D' -> [4]
      'E' -> [5]
      'F' -> [6]
      'G' -> [7]
      'H' -> [8]
      _   -> []
    row <- case rank of
      '1' -> [1]
      '2' -> [2]
      '3' -> [3]
      '4' -> [4]
      '5' -> [5]
      '6' -> [6]
      '7' -> [7]
      '8' -> [8]
      _   -> []
    return (Position row col, "")
  readsPrec _ _ = []

instance Read Move where
  readsPrec _ [fromFile, fromRank, toFile, toRank] = do
    (from, _) <- reads [fromFile, fromRank]
    (to, _)   <- reads [toFile, toRank]
    return (Move from to, "")
  readsPrec _ _ = []
