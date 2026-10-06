module Main where

import Board
import Text.Read
import qualified Data.Set as S

main :: IO ()
main = do
  print initialBoard
  putStrLn $ reverse $ show initialBoard
  let pieces color = map (getSquare initialBoard) (S.toList $ piecePositions initialBoard color)
  print $ pieces Black
  print $ pieces White
  input <- readMaybe <$> getLine
  case input of
    Just move -> print $ nextBoard initialBoard move
    Nothing   -> putStrLn "Invalid move"
