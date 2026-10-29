module Stockfischer.AI where

import Stockfischer.Rules
import Stockfischer.Board
import Stockfischer.Players
import qualified Data.Map as M

chooseMove :: LegalMoves -> LegalMove
chooseMove = head . M.toList

evaluateBoard :: Board -> Double
evaluateBoard board =
  getMaterialValue board White - getMaterialValue board Black

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
