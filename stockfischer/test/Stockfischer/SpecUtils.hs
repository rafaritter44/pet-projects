module Stockfischer.SpecUtils (playMoves, getLegalMoves) where

import Stockfischer.Rules
  ( GameState(PlayerTurn, GameOver)
  , LegalMoves
  , getGameState
  )
import Stockfischer.Board (Game, initialBoard)
import Stockfischer.Moves (Move)
import Control.Monad (foldM)
import qualified Data.Map as M (lookup)

playMoves :: [Move] -> Either String Game
playMoves = foldM playMove [initialBoard]

playMove :: Game -> Move -> Either String Game
playMove game move = do
  legalMoves <- getLegalMoves game
  case M.lookup move legalMoves of
    Just board -> Right (board:game)
    Nothing    -> Left $ "Unexpected illegal move: " ++ show move

getLegalMoves :: Game -> Either String LegalMoves
getLegalMoves game = case getGameState game of
  PlayerTurn legalMoves -> Right legalMoves
  GameOver result       -> Left $ "Game ended unexpectedly: " ++ show result
