module Stockfischer.Rules where

import Stockfischer.Board
import Stockfischer.Moves
import qualified Data.Map as M
import qualified Data.Set as S

getNextLegalBoards :: Board -> Color -> M.Map Move Board
getNextLegalBoards _ _ = M.empty

isInCheck :: Board -> Color -> Bool
isInCheck board color =
  getKingPosition board color `S.member` getAllAttackedPositions board (getNextPlayer color)

getNextPlayer :: Color -> Color
getNextPlayer White = Black
getNextPlayer Black = White

getAllAttackedPositions :: Board -> Color -> S.Set Position
getAllAttackedPositions board color = S.unions
  [ getAttackedPositions board position
  | position <- getPiecePositions board color
  ]

getAttackedPositions :: Board -> Position -> S.Set Position
getAttackedPositions board position = S.fromList $ case getSquare board position of
  Empty -> []
  Occupied (Piece White Pawn) -> getRelativePositions' whitePawnAttackOffsets
  Occupied (Piece Black Pawn) -> getRelativePositions' blackPawnAttackOffsets
  Occupied (Piece _ Knight)   -> getRelativePositions' knightOffsets
  Occupied (Piece _ Bishop)   -> getPositionsInDirections' diagonalOffsets
  Occupied (Piece _ Rook)     -> getPositionsInDirections' orthogonalOffsets
  Occupied (Piece _ Queen)    -> getPositionsInDirections' adjacentOffsets
  Occupied (Piece _ King)     -> getRelativePositions' adjacentOffsets
  where
    getRelativePositions'     = getRelativePositions position
    getPositionsInDirections' = getPositionsInDirections position

getReachablePositions :: Board -> Position -> [Position]
getReachablePositions board from =
  filter (isPositionReachable board from) (getNextCandidatePositions board from)

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
  Occupied (Piece _ Pawn)   -> isPositionAvailable' && isPathClear' -- TODO: FIXME
  Occupied (Piece _ Knight) -> isPositionAvailable'
  Occupied (Piece _ Bishop) -> isPositionAvailable' && isPathClear'
  Occupied (Piece _ Rook)   -> isPositionAvailable' && isPathClear'
  Occupied (Piece _ Queen)  -> isPositionAvailable' && isPathClear'
  Occupied (Piece _ King)   -> isPositionAvailable'
  where
    isPositionAvailable' = isPositionAvailable board from to
    isPathClear'         = isPathClear board from to

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
