module Rules where

import Board
import Move
import qualified Data.Map as M
import qualified Data.Set as S

getNextLegalBoards :: Board -> Color -> M.Map Move Board
getNextLegalBoards _ _ = M.empty

getAllAttackedPositions :: Board -> Color -> S.Set Position
getAllAttackedPositions _ _ = S.empty

getAttackedPositions :: Board -> Position -> S.Set Position
getAttackedPositions board position = case getSquare board position of
  Empty -> S.empty
  Occupied (Piece color Pawn)   -> S.empty
  Occupied (Piece color Knight) -> S.empty
  Occupied (Piece color Bishop) -> S.empty
  Occupied (Piece color Rook)   -> S.empty
  Occupied (Piece color Queen)  -> S.empty
  Occupied (Piece color King)   -> S.empty

getReachablePositions :: Board -> Position -> [Position]
getReachablePositions board position = case getSquare board position of
  Empty -> []
  Occupied (Piece color Pawn)   -> []
  Occupied (Piece color Knight) -> []
  Occupied (Piece color Bishop) -> []
  Occupied (Piece color Rook)   -> []
  Occupied (Piece color Queen)  -> []
  Occupied (Piece color King)   -> []

-- | Returns the positions a piece could move to on all possible boards.
getNextCandidatePositions :: Board -> Position -> [Position]
getNextCandidatePositions board position = case getSquare board position of
  Empty -> []
  Occupied piece@(Piece White Pawn)
    | isInitialPosition' piece -> getRelativePositions' initialWhitePawnOffsets
    | otherwise                -> getRelativePositions' whitePawnOffsets
  Occupied piece@(Piece Black Pawn)
    | isInitialPosition' piece -> getRelativePositions' initialBlackPawnOffsets
    | otherwise                -> getRelativePositions' blackPawnOffsets
  Occupied (Piece _ Knight) -> getRelativePositions' knightOffsets
  Occupied (Piece _ Bishop) -> getPositionsInDirections' diagonalOffsets
  Occupied (Piece _ Rook)   -> getPositionsInDirections' orthogonalOffsets
  Occupied (Piece _ Queen)  -> getPositionsInDirections' adjacentOffsets
  Occupied (Piece _ King)   -> getRelativePositions' adjacentOffsets
  where
    isInitialPosition'        = isInitialPosition position . Occupied
    getRelativePositions'     = getRelativePositions position
    getPositionsInDirections' = getPositionsInDirections position

isPositionReachable :: Board -> Position -> Position -> Bool
isPositionReachable board from to = case getSquare board from of
  Empty -> False
  Occupied (Piece color Pawn)   -> False
  Occupied (Piece color Knight) -> False
  Occupied (Piece color Bishop) -> False
  Occupied (Piece color Rook)   -> False
  Occupied (Piece color Queen)  -> False
  Occupied (Piece color King)   -> False

isPositionAvailable :: Board -> Position -> Position -> Bool
isPositionAvailable board from to = case (getSquare' from, getSquare' to) of
  (Occupied _, Empty) -> True
  (Occupied (Piece fromColor _), Occupied (Piece toColor _)) -> fromColor /= toColor
  _ -> False
  where
    getSquare' = getSquare board

isPathClear :: Board -> Position -> Position -> Bool
isPathClear board from to =
  all ((== Empty) . getSquare board) $ getPositionsBetween from to

isInCheck :: Board -> Color -> Bool
isInCheck _ _ = False
