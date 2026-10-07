module Game where

import Board
import Text.Read

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
      let nextBoard = getNextBoard currentBoard move
      let nextPlayer = getNextPlayer currentPlayer
      gameLoop nextBoard nextPlayer
    Nothing -> do
      putStrLn "Invalid input. Example of a valid input: e2e4"
      gameLoop currentBoard currentPlayer

getNextPlayer :: Color -> Color
getNextPlayer White = Black
getNextPlayer Black = White
