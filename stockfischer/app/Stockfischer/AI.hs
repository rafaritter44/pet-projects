module Stockfischer.AI where

import Stockfischer.Rules
import qualified Data.Map as M

getBestMove :: LegalMoves -> LegalMove
getBestMove = head . M.toList
