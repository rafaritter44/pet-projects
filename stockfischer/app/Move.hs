module Move where

import Data.Char
import Data.Maybe

data Position = Position Int Int
  deriving (Eq, Ord)
data Move = Move Position Position
data Offset = Offset Int Int

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

getRelativePositions :: Position -> [Offset] -> [Position]
getRelativePositions = mapMaybe . getRelativePosition

getRelativePosition :: Position -> Offset -> Maybe Position
getRelativePosition (Position row col) (Offset rowOffset colOffset)
  | isOnBoard position = Just position
  | otherwise          = Nothing
  where
    position = Position (row + rowOffset) (col + colOffset)

getPositionsInDirection :: Position -> Offset -> [Position]
getPositionsInDirection position offset =
  case getRelativePosition position offset of
    Just nextPosition -> nextPosition : getPositionsInDirection nextPosition offset
    Nothing           -> []

isOnBoard :: Position -> Bool
isOnBoard (Position row col) = isWithinBounds row && isWithinBounds col

isWithinBounds :: Int -> Bool
isWithinBounds coordinate = coordinate >= 1 && coordinate <= 8
