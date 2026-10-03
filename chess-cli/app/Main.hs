module Main where

import Board
import Move
import Data.Matrix
import Data.Maybe
import qualified Data.Set as S

main :: IO ()
main = do
  putStrLn $ boardString White initialBoard
  putStrLn $ boardString Black initialBoard
  let pieces color = map (initialBoard !) (S.toList $ piecePositions initialBoard color)
  print $ pieces Black
  print $ pieces White
  let move = fromJust $ parseMove "e2e4"
  putStrLn $ boardString White $ nextBoard initialBoard move
