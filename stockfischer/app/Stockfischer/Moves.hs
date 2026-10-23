module Stockfischer.Moves where

import Stockfischer.Players
import Data.Char
import Data.Maybe
import Data.List

type Coordinate = Int

data Position = Position Coordinate Coordinate
  deriving (Eq, Ord)
data Move = Move Position Position
  deriving (Eq, Ord)
data Offset = Offset Int Int

data CastlingSide = Kingside | Queenside
data Castling = Castling Color CastlingSide

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
diagonalOffsets   = [upLeft, upRight, downLeft, downRight]
adjacentOffsets   = orthogonalOffsets ++ diagonalOffsets

twoUp, twoDown :: Offset
twoUp   = Offset 2 0
twoDown = Offset (-2) 0

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

getPawnOffsets :: Color -> [Offset]
getPawnOffsets White = [up, upLeft, upRight]
getPawnOffsets Black = [down, downLeft, downRight]

getInitialPawnOffsets :: Color -> [Offset]
getInitialPawnOffsets White = twoUp : getPawnOffsets White
getInitialPawnOffsets Black = twoDown : getPawnOffsets Black

getKingInitialPosition :: Color -> Position
getKingInitialPosition White = Position 1 5
getKingInitialPosition Black = Position 8 5

-- | The position the king must pass through when castling.
getIntermediateCastlingPosition :: Castling -> Position
getIntermediateCastlingPosition = getRookCastlingPosition

getIntermediateCastlingMove :: Castling -> Move
getIntermediateCastlingMove castling@(Castling color _) =
  Move (getKingInitialPosition color) (getIntermediateCastlingPosition castling)

getRookCastlingMove :: Castling -> Move
getRookCastlingMove castling =
  Move (getRookInitialPosition castling) (getRookCastlingPosition castling)

getRookInitialPosition :: Castling -> Position
getRookInitialPosition (Castling White Kingside)  = Position 1 8
getRookInitialPosition (Castling White Queenside) = Position 1 1
getRookInitialPosition (Castling Black Kingside)  = Position 8 8
getRookInitialPosition (Castling Black Queenside) = Position 8 1

getRookCastlingPosition :: Castling -> Position
getRookCastlingPosition (Castling White Kingside)  = Position 1 6
getRookCastlingPosition (Castling White Queenside) = Position 1 4
getRookCastlingPosition (Castling Black Kingside)  = Position 8 6
getRookCastlingPosition (Castling Black Queenside) = Position 8 4

getCastlingPositions :: Color -> [Position]
getCastlingPositions color =
  [ getCastlingPosition (Castling color side)
  | side <- [Kingside, Queenside]
  ]

getCastlingPosition :: Castling -> Position
getCastlingPosition (Castling White Kingside)  = Position 1 7
getCastlingPosition (Castling White Queenside) = Position 1 3
getCastlingPosition (Castling Black Kingside)  = Position 8 7
getCastlingPosition (Castling Black Queenside) = Position 8 3

getCastling :: Color -> Position -> Position -> Maybe Castling
getCastling color from to
  | from /= getKingInitialPosition color = Nothing
  | otherwise = find ((== to) . getCastlingPosition) castlings
  where
    castlings = map (Castling color) [Kingside, Queenside]

getRelativePositions :: Position -> [Offset] -> [Position]
getRelativePositions = mapMaybe . getRelativePosition

getRelativePosition :: Position -> Offset -> Maybe Position
getRelativePosition (Position row col) (Offset rowOffset colOffset)
  | isOnBoard position = Just position
  | otherwise          = Nothing
  where
    position = Position (row + rowOffset) (col + colOffset)

isOnBoard :: Position -> Bool
isOnBoard (Position row col) = isWithinBounds row && isWithinBounds col

isWithinBounds :: Coordinate -> Bool
isWithinBounds coordinate = coordinate >= 1 && coordinate <= 8

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

isSameColumn :: Position -> Position -> Bool
isSameColumn (Position _ col1) (Position _ col2) = col1 == col2

isSameSquareColor :: Position -> Position -> Bool
isSameSquareColor (Position row1 col1) (Position row2 col2) =
  odd (row1 + col1) == odd (row2 + col2)
