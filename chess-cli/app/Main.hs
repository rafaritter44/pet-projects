module Main where

import Board
import Data.Matrix
import qualified Data.Set as S

main :: IO ()
main = do
  print initialBoard
  let pieces color = map (initialBoard !) (S.toList $ piecePositions initialBoard color)
  print $ pieces Black
  print $ pieces White
