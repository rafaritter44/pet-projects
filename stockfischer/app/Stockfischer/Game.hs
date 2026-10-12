module Stockfischer.Game where

import Stockfischer.Rules
import Stockfischer.Board
import Text.Read
import qualified Data.Map as M

gameLoop :: Board -> Color -> IO ()
gameLoop currentBoard currentPlayer = do
  let nextLegalBoards = getNextLegalBoards currentBoard currentPlayer
  if M.null nextLegalBoards
    then do
      let result = if isInCheck currentBoard currentPlayer
          then "Checkmate. " ++ show nextPlayer ++ " won."
          else "Stalemate."
      putStrLn $ "Game over: " ++ result
    else do
      putStrLn $ show currentPlayer ++ "'s turn."
      case currentPlayer of
        White -> print currentBoard
        Black -> putStrLn $ reverse $ show currentBoard
      putStrLn "Enter your move:"
      input <- readMaybe <$> getLine
      case input of
        Just move -> do
          case M.lookup move nextLegalBoards of
            Just nextBoard -> gameLoop nextBoard nextPlayer
            Nothing        -> retryTurn' "Illegal move. Try again."
        Nothing -> retryTurn' "Invalid input. Example of a valid input: e2e4"
  where
    nextPlayer = getNextPlayer currentPlayer
    retryTurn' = retryTurn currentBoard currentPlayer

retryTurn :: Board -> Color -> String -> IO ()
retryTurn board player message = do
  putStrLn message
  gameLoop board player
