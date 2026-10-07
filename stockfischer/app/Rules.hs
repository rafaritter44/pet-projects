module Rules where

import Board
import Move
import qualified Data.Map as M
import qualified Data.Set as S

getNextLegalBoards :: Board -> Color -> M.Map Move Board
getNextLegalBoards _ _ = M.empty

getAttackedPositions :: Board -> Color -> S.Set Position
getAttackedPositions _ _ = S.empty

getAttackedPositionsFrom :: Board -> Position -> S.Set Position
getAttackedPositionsFrom _ _ = S.empty

getReachablePositionsFrom :: Board -> Position -> S.Set Position
getReachablePositionsFrom _ _ = S.empty

isInCheck :: Board -> Color -> Bool
isInCheck _ _ = False
