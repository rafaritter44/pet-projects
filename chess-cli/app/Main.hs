module Main where

import Board
import Data.Matrix
import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn $ boardString White initialBoard
  putStrLn $ boardString Black initialBoard
  let pieces color = map (initialBoard !) (S.toList $ piecePositions initialBoard color)
  print $ pieces Black
  print $ pieces White
  putStrLn $ boardString White $ nextBoard initialBoard ((2, 5), (4, 5))
