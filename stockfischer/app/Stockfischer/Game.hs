module Stockfischer.Game where

import Stockfischer.Rules
import Stockfischer.Board
import Text.Read
import qualified Data.Map as M

gameLoop :: Board -> Color -> IO ()
gameLoop currentBoard currentPlayer = do
  putStrLn $ show currentPlayer ++ "'s turn."
  case currentPlayer of
    White -> print currentBoard
    Black -> putStrLn $ reverse $ show currentBoard
  putStrLn "Enter your move:"
  input <- readMaybe <$> getLine
  case input of
    Just move -> do
      let nextLegalBoards = getNextLegalBoards currentBoard currentPlayer
      case M.lookup move nextLegalBoards of
        Just nextBoard -> do
          let nextPlayer = getNextPlayer currentPlayer
          gameLoop nextBoard nextPlayer
        Nothing -> do
          putStrLn "Illegal move. Try again."
          gameLoop currentBoard currentPlayer
    Nothing -> do
      putStrLn "Invalid input. Example of a valid input: e2e4"
      gameLoop currentBoard currentPlayer
