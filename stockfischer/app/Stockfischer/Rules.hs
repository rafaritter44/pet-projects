module Stockfischer.Rules where

import Stockfischer.Board
import Stockfischer.Moves
import qualified Data.Map as M

data DrawReason = Stalemate | InsufficientMaterial | ThreefoldRepetition | FiftyMoveRule
data GameResult = Checkmate | Draw DrawReason
data GameState = GameOver GameResult | PlayerTurn LegalMoves

type LegalMoves = M.Map Move Board

instance Show DrawReason where
  show Stalemate            = "Stalemate"
  show InsufficientMaterial = "Draw by insufficient material"
  show ThreefoldRepetition  = "Draw by threefold repetition"
  show FiftyMoveRule        = "Draw by the fifty-move rule"

getGameState :: Game -> GameState
getGameState game
  | M.null legalMoves = GameOver $ if isInCheck game player
    then Checkmate
    else Draw Stalemate
  | otherwise = PlayerTurn legalMoves
  where
    legalMoves = M.fromList
      [ (move, nextBoard)
      | from <- getPiecePositions board player
      , to   <- getReachablePositions game from
      , let move      = Move from to
      , let nextBoard = getNextBoard board move
      , not $ isInCheck (nextBoard:game) player
      ]
    board  = getCurrentBoard game
    player = getCurrentPlayer game

isInCheck :: Game -> Color -> Bool
isInCheck game player =
  isPositionReachableByAny game kingPosition nextPlayer
  where
    kingPosition = getKingPosition (getCurrentBoard game) player
    nextPlayer   = getNextPlayer player

isPositionReachableByAny :: Game -> Position -> Color -> Bool
isPositionReachableByAny game position color =
  any (containsPosition . getReachablePositions') piecePositions
  where
    containsPosition       = elem position
    getReachablePositions' = getReachablePositions game
    piecePositions         = getPiecePositions (getCurrentBoard game) color

getReachablePositions :: Game -> Position -> [Position]
getReachablePositions game from = filter isReachable candidatePositions
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

    getSquare'                = getSquare $ getCurrentBoard game
    isInitialPosition'        = isInitialPosition from . Occupied
    getRelativePositions'     = getRelativePositions from
    getPositionsInDirections' = getPositionsInDirections from
