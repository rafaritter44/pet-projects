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

up, down, left, right :: Offset
up    = Offset (-1) 0
down  = Offset 1 0
left  = Offset 0 (-1)
right = Offset 0 1

upLeft, upRight, downLeft, downRight :: Offset
upLeft    = Offset (-1) (-1)
upRight   = Offset (-1) 1
downLeft  = Offset 1 (-1)
downRight = Offset 1 1

orthogonalOffsets, diagonalOffsets, adjacentOffsets :: [Offset]
orthogonalOffsets = [up, down, left, right]
diagonalOffsets = [upLeft, upRight, downLeft, downRight]
adjacentOffsets = orthogonalOffsets ++ diagonalOffsets

knightUpLeft, knightUpRight, knightLeftUp, knightRightUp :: Offset
knightLeftDown, knightRightDown, knightDownLeft, knightDownRight :: Offset
knightUpLeft    = Offset (-2) (-1)
knightUpRight   = Offset (-2) 1
knightLeftUp    = Offset (-1) (-2)
knightRightUp   = Offset (-1) 2
knightLeftDown  = Offset 1 (-2)
knightRightDown = Offset 1 2
knightDownLeft  = Offset 2 (-1)
knightDownRight = Offset 2 1

knightOffsets :: [Offset]
knightOffsets =
  [ knightUpLeft, knightUpRight
  , knightLeftUp, knightRightUp
  , knightLeftDown, knightRightDown
  , knightDownLeft, knightDownRight
  ]

getRelativePositions :: Position -> [Offset] -> [Position]
getRelativePositions = mapMaybe . getRelativePosition

getRelativePosition :: Position -> Offset -> Maybe Position
getRelativePosition (Position row col) (Offset rowOffset colOffset)
  | isOnBoard position = Just position
  | otherwise          = Nothing
  where
    position = Position (row + rowOffset) (col + colOffset)

getPositionsInDirections :: Position -> [Offset] -> [Position]
getPositionsInDirections = concatMap . getPositionsInDirection

getPositionsInDirection :: Position -> Offset -> [Position]
getPositionsInDirection position offset =
  case getRelativePosition position offset of
    Just nextPosition -> nextPosition : getPositionsInDirection nextPosition offset
    Nothing           -> []

isOnBoard :: Position -> Bool
isOnBoard (Position row col) = isWithinBounds row && isWithinBounds col

isWithinBounds :: Int -> Bool
isWithinBounds coordinate = coordinate >= 1 && coordinate <= 8
