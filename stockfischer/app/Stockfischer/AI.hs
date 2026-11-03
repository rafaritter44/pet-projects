module Stockfischer.AI where

import Stockfischer.Rules
import Stockfischer.Board
import Stockfischer.Moves hiding (Queen, Rook, Bishop, Knight)
import Stockfischer.Players
import Data.List
import Data.Ord
import qualified Data.Map as M

searchDepth :: Int
searchDepth = 3

infinity :: Double
infinity = 1 / 0

pieceDevelopmentValue, castlingValue :: Double
pieceDevelopmentValue = 0.1
castlingValue = 0.5

chooseMove :: Game -> LegalMoves -> LegalMove
chooseMove game legalMoves =
  fst $ maximumBy (comparing snd)
    [ ((move, board), - negamax searchDepth (- infinity) infinity (board:game))
    | (move, board) <- M.toList legalMoves
    ]

negamax :: Int -> Double -> Double -> Game -> Double
negamax depth alpha beta game = case getGameState game of
  GameOver (Draw _)  -> 0
  GameOver Checkmate -> - infinity
  PlayerTurn legalMoves
    | depth <= 0 -> evaluateGame game
    | otherwise  -> search alpha $ M.elems legalMoves
  where
    search a [] = a
    search a (board:boards)
      | a >= beta = a
      | otherwise = search (max a score) boards
      where
        score = - negamax (depth - 1) (- beta) (- a) (board:game)

evaluateGame :: Game -> Double
evaluateGame game =
  getTotalValue board player - getTotalValue board (getNextPlayer player)
  where
    board  = getCurrentBoard game
    player = getCurrentPlayer game

getTotalValue :: Board -> Color -> Double
getTotalValue board player =
  getMaterialValue board player + getDevelopmentValue board player

getDevelopmentValue :: Board -> Color -> Double
getDevelopmentValue board player =
  getPieceDevelopmentValue board player + getCastlingValue board player

getPieceDevelopmentValue :: Board -> Color -> Double
getPieceDevelopmentValue board player = pieceDevelopmentValue * fromIntegral (length
  [ position
  | pieceType <- [Knight, Bishop]
  , position  <- getPieceTypePositions board player pieceType
  , not $ isInitialPosition position $ getSquare board position
  ])

getCastlingValue :: Board -> Color -> Double
getCastlingValue board player
 | getKingPosition board player `elem` getCastlingPositions player = castlingValue
 | otherwise = 0

getMaterialValue :: Board -> Color -> Double
getMaterialValue board player =
  sum $ map getPieceValue $ getPieceTypes board player

getPieceValue :: PieceType -> Double
getPieceValue Pawn   = 1
getPieceValue Knight = 3
getPieceValue Bishop = 3.25
getPieceValue Rook   = 5
getPieceValue Queen  = 9
getPieceValue King   = 0
