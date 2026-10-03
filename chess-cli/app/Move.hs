module Move where

import Data.Char

type Position = (Int, Int)
type Move = (Position, Position)

parseMove :: String -> Maybe Move
parseMove [fromFile, fromRank, toFile, toRank] = do
  from <- parsePosition fromFile fromRank
  to   <- parsePosition toFile toRank
  return (from, to)
parseMove _ = Nothing

parsePosition :: Char -> Char -> Maybe Position
parsePosition file rank = do
  col <- fileToCol file
  row <- rankToRow rank
  return (row, col)

fileToCol :: Char -> Maybe Int
fileToCol file =
  case toUpper file of
    'A' -> Just 1
    'B' -> Just 2
    'C' -> Just 3
    'D' -> Just 4
    'E' -> Just 5
    'F' -> Just 6
    'G' -> Just 7
    'H' -> Just 8
    _   -> Nothing

rankToRow :: Char -> Maybe Int
rankToRow '1' = Just 1
rankToRow '2' = Just 2
rankToRow '3' = Just 3
rankToRow '4' = Just 4
rankToRow '5' = Just 5
rankToRow '6' = Just 6
rankToRow '7' = Just 7
rankToRow '8' = Just 8
rankToRow _   = Nothing
