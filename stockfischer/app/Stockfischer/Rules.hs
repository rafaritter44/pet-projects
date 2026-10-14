module Stockfischer.Rules where

import Stockfischer.Board
import Stockfischer.Moves
import qualified Data.Map as M

getNextLegalBoards :: Board -> Color -> M.Map Move Board
getNextLegalBoards board player = M.fromList
  [ (move, nextBoard)
  | from <- getPiecePositions board player
  , to   <- getReachablePositions board from
  , let move      = Move from to
  , let nextBoard = getNextBoard board move
  , not $ isInCheck nextBoard player
  ]

isInCheck :: Board -> Color -> Bool
isInCheck board player =
  isPositionReachableByAny board kingPosition nextPlayer
  where
    kingPosition = getKingPosition board player
    nextPlayer   = getNextPlayer player

getNextPlayer :: Color -> Color
getNextPlayer White = Black
getNextPlayer Black = White

isPositionReachableByAny :: Board -> Position -> Color -> Bool
isPositionReachableByAny board position color =
  any (containsPosition . getReachablePositions') piecePositions
  where
    containsPosition       = elem position
    getReachablePositions' = getReachablePositions board
    piecePositions         = getPiecePositions board color

getReachablePositions :: Board -> Position -> [Position]
getReachablePositions board from = filter isReachable candidatePositions
  where
    isReachable to = case getSquare' from of
      Empty                     -> False
      Occupied (Piece _ Knight) -> isAvailable to
      Occupied (Piece _ _)      -> isAvailable to && isPathClear to

    isAvailable to = case (getSquare' from, getSquare' to) of
      (Occupied (Piece _ Pawn), Empty) -> isSameColumn from to
      (Occupied _             , Empty) -> True
      (Occupied (Piece fromColor Pawn), Occupied (Piece toColor _)) ->
        fromColor /= toColor && not (isSameColumn from to)
      (Occupied (Piece fromColor _)   , Occupied (Piece toColor _)) ->
        fromColor /= toColor
      _ -> False

    isPathClear to = all ((== Empty) . getSquare') $ getPositionsBetween from to

    -- The positions a piece could move to on all possible boards.
    candidatePositions = case getSquare' from of
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

    getSquare'                = getSquare board
    isInitialPosition'        = isInitialPosition from . Occupied
    getRelativePositions'     = getRelativePositions from
    getPositionsInDirections' = getPositionsInDirections from
