module Main where

import Board
import Data.Matrix
import qualified Data.Set as S

main :: IO ()
main = do
  printBoard White initialBoard
  printBoard Black initialBoard
  let pieces color = map (initialBoard !) (S.toList $ piecePositions initialBoard color)
  print $ pieces Black
  print $ pieces White
  printBoard White $ nextBoard initialBoard ((7, 5), (5, 5))
