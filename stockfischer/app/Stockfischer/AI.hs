module Stockfischer.AI where

import Stockfischer.Rules
import Stockfischer.Board
import Stockfischer.Players
import qualified Data.Map as M

chooseMove :: LegalMoves -> LegalMove
chooseMove = head . M.toList

negamax :: Int -> Game -> Double
negamax depth game = case getGameState game of
  GameOver (Draw _)  -> 0
  GameOver Checkmate -> (-1) / 0
  PlayerTurn legalMoves
    | depth <= 0 -> evaluateGame game
    | otherwise  -> maximum
      [ negate $ negamax (depth - 1) (board:game)
      | board <- M.elems legalMoves
      ]

evaluateGame :: Game -> Double
evaluateGame game =
  getMaterialValue board player - getMaterialValue board (getNextPlayer player)
  where
    board  = getCurrentBoard game
    player = getCurrentPlayer game

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
