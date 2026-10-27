module Stockfischer.Rules where

import Stockfischer.Board
import Stockfischer.Moves hiding (Queen, Rook, Bishop, Knight)
import Stockfischer.Players
import qualified Data.Map as M

data DrawReason = Stalemate | InsufficientMaterial | ThreefoldRepetition | FiftyMoveRule
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
  | isInsufficientMaterial board = GameOver $ Draw InsufficientMaterial
  | isThreefoldRepetition game   = GameOver $ Draw ThreefoldRepetition
  | isFiftyMoveDraw game         = GameOver $ Draw FiftyMoveRule
  | M.null legalMoves = GameOver $ if isInCheck game player
    then Checkmate
    else Draw Stalemate
  | otherwise = PlayerTurn legalMoves
  where
    legalMoves = M.fromList
      [ (move, nextBoard)
      | from <- getPiecePositions board player
      , to   <- getReachablePositions game from
      , move <- getMoveOrPromotions board from to
      , let nextBoard = playMove board move
      , not $ isInCheck (nextBoard:game) player
      ]
    board  = getCurrentBoard game
    player = getCurrentPlayer game

isInCheck :: Game -> Color -> Bool
isInCheck game player =
  any (elem kingPosition . getReachablePositions game) opponentPiecePositions
  where
    kingPosition           = getKingPosition board player
    opponentPiecePositions = getPiecePositions board $ getNextPlayer player
    board                  = getCurrentBoard game

getReachablePositions :: Game -> Position -> [Position]
getReachablePositions game from = filter isReachable candidatePositions
  where
    isReachable to = case getSquare' from of
      Empty -> False
      Occupied (Piece color King)
        | Just castling <- getCastling color from to -> isCastlingLegal game castling
        | otherwise                                  -> isAvailable to
      Occupied (Piece _ Knight) -> isAvailable to
      Occupied (Piece _ _)      -> isAvailable to && isPathClear board from to

    isAvailable to = case (getSquare' from, getSquare' to) of
      (Occupied (Piece _ Pawn), Empty) ->
        isSameColumn from to || isEnPassantLegal game from to
      (Occupied _             , Empty) -> True
      (Occupied (Piece fromColor Pawn), Occupied (Piece toColor _)) ->
        fromColor /= toColor && not (isSameColumn from to)
      (Occupied (Piece fromColor _)   , Occupied (Piece toColor _)) ->
        fromColor /= toColor
      _ -> False

    -- The positions a piece could move to on all possible boards.
    candidatePositions = case getSquare' from of
      Empty -> []
      Occupied piece@(Piece color Pawn)
        | isInitialPosition' piece -> getRelativePositions' $ getPawnInitialOffsets color
        | otherwise                -> getRelativePositions' $ getPawnOffsets color
      Occupied piece@(Piece color King)
        | isInitialPosition' piece -> adjacentPositions ++ getCastlingPositions color
        | otherwise                -> adjacentPositions
      Occupied (Piece _ Knight) -> getRelativePositions' knightOffsets
      Occupied (Piece _ Bishop) -> getPositionsInDirections' diagonalOffsets
      Occupied (Piece _ Rook)   -> getPositionsInDirections' orthogonalOffsets
      Occupied (Piece _ Queen)  -> getPositionsInDirections' adjacentOffsets

    board                     = getCurrentBoard game
    getSquare'                = getSquare board
    isInitialPosition'        = isInitialPosition from . Occupied
    getRelativePositions'     = getRelativePositions from
    getPositionsInDirections' = getPositionsInDirections from
    adjacentPositions         = getRelativePositions' adjacentOffsets

isCastlingLegal :: Game -> Castling -> Bool
isCastlingLegal game castling@(Castling color _) =
  player == color
  && not (hasEverMoved' kingPosition)
  && not (hasEverMoved' rookPosition)
  && isPathClear board kingPosition rookPosition
  && not (isInCheck game player)
  && not (isInCheck (intermediateBoard:game) player)
  where
    board             = getCurrentBoard game
    player            = getCurrentPlayer game
    hasEverMoved'     = hasEverMoved game
    kingPosition      = getKingInitialPosition color
    rookPosition      = getRookInitialPosition castling
    intermediateBoard = playMove board $ getIntermediateCastlingMove castling

isEnPassantLegal :: Game -> Position -> Position -> Bool
isEnPassantLegal game from to
  | Just previousBoard <- getPreviousBoard game =
      getSquare previousBoard opponentPawnInitialPosition   == Occupied (Piece opponent Pawn)
      && getSquare previousBoard opponentPawnPosition       == Empty
      && getSquare currentBoard opponentPawnInitialPosition == Empty
      && getSquare currentBoard opponentPawnPosition        == Occupied (Piece opponent Pawn)
  | otherwise = False
  where
    currentBoard                = getCurrentBoard game
    opponent                    = getNextPlayer $ getCurrentPlayer game
    opponentPawnPosition        = getEnPassantOpponentPosition from to
    opponentPawnColumn          = getColumn opponentPawnPosition
    opponentPawnInitialPosition = getPawnInitialPosition opponent opponentPawnColumn

isInsufficientMaterial :: Board -> Bool
isInsufficientMaterial board = case nonKingPieces of
  []            -> True
  [(_, Bishop)] -> True
  [(_, Knight)] -> True
  [(position1, Bishop), (position2, Bishop)] -> isSameSquareColor position1 position2
  _ -> False
  where
    nonKingPieces =
      [ (position, pieceType)
      | position <- getAllPiecePositions board
      , Occupied (Piece _ pieceType) <- [getSquare board position]
      , pieceType /= King
      ]

isThreefoldRepetition :: Game -> Bool
isThreefoldRepetition game =
  length (filter (== getCurrentBoard game) game) >= 3

isFiftyMoveDraw :: Game -> Bool
isFiftyMoveDraw = isFiftyMoveDraw' 0
  where
    isFiftyMoveDraw' :: Int -> Game -> Bool
    isFiftyMoveDraw' count (currentBoard:previousBoard:previousBoards)
      | isCaptureOrPawnMove currentBoard previousBoard = False
      | count == 99 = True
      | otherwise   = isFiftyMoveDraw' (count + 1) (previousBoard:previousBoards)
    isFiftyMoveDraw' _ _ = False
