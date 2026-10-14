module Stockfischer.Moves where

import Data.Char
import Data.Maybe

type Coordinate = Int

data Position = Position Coordinate Coordinate
  deriving (Eq, Ord)
data Move = Move Position Position
  deriving (Eq, Ord)
data Offset = Offset Int Int

instance Read Position where
  readsPrec _ [file, rank] = do
    col <- case toLower file of
      'a' -> [1]
      'b' -> [2]
      'c' -> [3]
      'd' -> [4]
      'e' -> [5]
      'f' -> [6]
      'g' -> [7]
      'h' -> [8]
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

instance Show Position where
  show (Position row col) = [file, rank]
    where
      file = "abcdefgh" !! (col - 1)
      rank = "12345678" !! (row - 1)

instance Show Move where
  show (Move from to) = show from ++ show to

up, down, left, right :: Offset
up    = Offset 1 0
down  = Offset (-1) 0
left  = Offset 0 (-1)
right = Offset 0 1

upLeft, upRight, downLeft, downRight :: Offset
upLeft    = Offset 1 (-1)
upRight   = Offset 1 1
downLeft  = Offset (-1) (-1)
downRight = Offset (-1) 1

orthogonalOffsets, diagonalOffsets, adjacentOffsets :: [Offset]
orthogonalOffsets = [up, down, left, right]
diagonalOffsets = [upLeft, upRight, downLeft, downRight]
adjacentOffsets = orthogonalOffsets ++ diagonalOffsets

twoUp, twoDown :: Offset
twoUp   = Offset 2 0
twoDown = Offset (-2) 0

whitePawnOffsets, blackPawnOffsets :: [Offset]
whitePawnOffsets = [up, upLeft, upRight]
blackPawnOffsets = [down, downLeft, downRight]

initialWhitePawnOffsets, initialBlackPawnOffsets :: [Offset]
initialWhitePawnOffsets = twoUp : whitePawnOffsets
initialBlackPawnOffsets = twoDown : blackPawnOffsets

knightUpLeft, knightUpRight, knightLeftUp, knightRightUp :: Offset
knightLeftDown, knightRightDown, knightDownLeft, knightDownRight :: Offset
knightUpLeft    = Offset 2 (-1)
knightUpRight   = Offset 2 1
knightLeftUp    = Offset 1 (-2)
knightRightUp   = Offset 1 2
knightLeftDown  = Offset (-1) (-2)
knightRightDown = Offset (-1) 2
knightDownLeft  = Offset (-2) (-1)
knightDownRight = Offset (-2) 1

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
    Just next -> next : getPositionsInDirection next offset
    Nothing   -> []

getPositionsBetween :: Position -> Position -> [Position]
getPositionsBetween from to
  | next == to = []
  | otherwise  = next : getPositionsBetween next to
  where
    next = getNextPositionBetween from to

getNextPositionBetween :: Position -> Position -> Position
getNextPositionBetween (Position fromRow fromCol) (Position toRow toCol) =
  Position
    (getNextCoordinateBetween fromRow toRow)
    (getNextCoordinateBetween fromCol toCol)

getNextCoordinateBetween :: Coordinate -> Coordinate -> Coordinate
getNextCoordinateBetween from to = from + signum (to - from)

isOnBoard :: Position -> Bool
isOnBoard (Position row col) = isWithinBounds row && isWithinBounds col

isWithinBounds :: Coordinate -> Bool
isWithinBounds coordinate = coordinate >= 1 && coordinate <= 8

isSameColumn :: Position -> Position -> Bool
isSameColumn (Position _ col1) (Position _ col2) = col1 == col2
