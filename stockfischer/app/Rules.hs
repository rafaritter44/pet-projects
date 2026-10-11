module Rules where

import Board
import Move
import qualified Data.Map as M
import qualified Data.Set as S

getNextLegalBoards :: Board -> Color -> M.Map Move Board
getNextLegalBoards _ _ = M.empty

getAttackedPositions :: Board -> Color -> S.Set Position
getAttackedPositions _ _ = S.empty

getAttackedPositionsFrom :: Board -> Position -> S.Set Position
getAttackedPositionsFrom board position = case getSquare board position of
  Empty -> S.empty
  Occupied (Piece color Pawn)   -> S.empty
  Occupied (Piece color Knight) -> S.empty
  Occupied (Piece color Bishop) -> S.empty
  Occupied (Piece color Rook)   -> S.empty
  Occupied (Piece color Queen)  -> S.empty
  Occupied (Piece color King)   -> S.empty

getReachablePositionsFrom :: Board -> Position -> [Position]
getReachablePositionsFrom board position = case getSquare board position of
  Empty -> []
  Occupied (Piece color Pawn)   -> []
  Occupied (Piece color Knight) -> []
  Occupied (Piece color Bishop) -> []
  Occupied (Piece color Rook)   -> []
  Occupied (Piece color Queen)  -> []
  Occupied (Piece color King)   -> []

-- | Returns the positions a piece could move to on all possible boards.
getNextCandidatePositionsFrom :: Board -> Position -> [Position]
getNextCandidatePositionsFrom board position = case getSquare board position of
  Empty -> []
  Occupied (Piece color Pawn) -> []
  Occupied (Piece _ Knight)   -> getRelativePositions position knightOffsets
  Occupied (Piece _ Bishop)   -> getPositionsInDirections position diagonalOffsets
  Occupied (Piece _ Rook)     -> getPositionsInDirections position orthogonalOffsets
  Occupied (Piece _ Queen)    -> getPositionsInDirections position adjacentOffsets
  Occupied (Piece _ King)     -> getRelativePositions position adjacentOffsets

isPositionReachableFrom :: Board -> Position -> Position -> Bool
isPositionReachableFrom board from to = case getSquare board from of
  Empty -> False
  Occupied (Piece color Pawn)   -> False
  Occupied (Piece color Knight) -> False
  Occupied (Piece color Bishop) -> False
  Occupied (Piece color Rook)   -> False
  Occupied (Piece color Queen)  -> False
  Occupied (Piece color King)   -> False

isInCheck :: Board -> Color -> Bool
isInCheck _ _ = False
