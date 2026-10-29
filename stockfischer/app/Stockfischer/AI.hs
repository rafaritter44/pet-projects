module Stockfischer.AI where

import Stockfischer.Rules
import Stockfischer.Board
import qualified Data.Map as M

chooseMove :: LegalMoves -> LegalMove
chooseMove = head . M.toList

getPieceValue :: PieceType -> Double
getPieceValue Pawn   = 1
getPieceValue Knight = 3
getPieceValue Bishop = 3.25
getPieceValue Rook   = 5
getPieceValue Queen  = 9
getPieceValue King   = 0
