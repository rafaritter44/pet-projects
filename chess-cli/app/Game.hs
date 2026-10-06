module Game where

import Board
import Text.Read

gameLoop :: Board -> IO ()
gameLoop board = do
  putStr "Enter your move: "
  input <- readMaybe <$> getLine
  case input of
    Just move -> do
      let nextBoard = getNextBoard board move
      print nextBoard
      gameLoop nextBoard
    Nothing -> do
      putStrLn "Invalid move."
      gameLoop board
