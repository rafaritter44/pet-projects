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

isInCheck :: Board -> Color -> Bool
isInCheck _ _ = False
