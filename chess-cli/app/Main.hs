module Main where

import Board
import Move
import Data.Maybe
import qualified Data.Set as S

main :: IO ()
main = do
  print initialBoard
  putStrLn $ reverse $ show initialBoard
  let pieces color = map (getSquare initialBoard) (S.toList $ piecePositions initialBoard color)
  print $ pieces Black
  print $ pieces White
  let move = fromJust $ parseMove "e2e4"
  print $ nextBoard initialBoard move
