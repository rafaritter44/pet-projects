module Stockfischer.Game where

import Stockfischer.Rules
import Stockfischer.Board
import Text.Read
import qualified Data.Map as M

data PlayerType = Human | Computer

gameLoop :: PlayerType -> PlayerType -> Color -> Board -> IO ()
gameLoop whitePlayerType blackPlayerType player board = do
  case player of
    White -> print board
    Black -> putStrLn $ reverse $ show board
  if M.null nextLegalBoards
    then putStrLn $ if isInCheck board player
      then "Checkmate. " ++ show nextPlayer ++ " won."
      else "Stalemate."
    else do
      putStrLn $ show player ++ "'s turn."
      putStrLn "Enter your move:"
      input <- readMaybe <$> getLine
      case input of
        Just move -> do
          case M.lookup move nextLegalBoards of
            Just nextBoard -> playNextTurn nextBoard
            Nothing        -> retryTurn "Illegal move. Try again."
        Nothing -> retryTurn "Invalid input. Example of a valid input: e2e4"
  where
    nextLegalBoards   = getNextLegalBoards board player
    nextPlayer        = getNextPlayer player
    playTurn          = gameLoop whitePlayerType blackPlayerType
    playNextTurn      = playTurn nextPlayer
    retryTurn message = do
      putStrLn message
      playTurn player board
