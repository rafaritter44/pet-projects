module Stockfischer.AI where

import Stockfischer.Rules
import qualified Data.Map as M

chooseMove :: LegalMoves -> LegalMove
chooseMove = head . M.toList
