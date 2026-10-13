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
isPositionReachableByAny board to color =
  any isPositionReachable' piecePositions
  where
    isPositionReachable' from = isPositionReachable board from to
    piecePositions            = getPiecePositions board color

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
  Empty                     -> False
  Occupied (Piece _ Knight) -> positionAvailable
  Occupied (Piece _ _)      -> positionAvailable && pathClear
  where
    positionAvailable = isPositionAvailable board from to
    pathClear         = isPathClear board from to

isPositionAvailable :: Board -> Position -> Position -> Bool
isPositionAvailable board from to = case (getSquare' from, getSquare' to) of
  (Occupied (Piece _ Pawn), Empty) -> sameColumn
  (Occupied _             , Empty) -> True
  (Occupied (Piece fromColor Pawn), Occupied (Piece toColor _)) -> fromColor /= toColor && not sameColumn
  (Occupied (Piece fromColor _)   , Occupied (Piece toColor _)) -> fromColor /= toColor
  _ -> False
  where
    getSquare' = getSquare board
    sameColumn = isSameColumn from to

isPathClear :: Board -> Position -> Position -> Bool
isPathClear board from to =
  all ((== Empty) . getSquare board) $ getPositionsBetween from to
