module Stockfischer.Game where

import Stockfischer.Rules
import Stockfischer.Board
import Text.Read
import qualified Data.Map as M

gameLoop :: Board -> Color -> IO ()
gameLoop board player = do
  case player of
    White -> print board
    Black -> putStrLn $ reverse $ show board
  if M.null nextLegalBoards
    then putStrLn $ if isInCheck board player
      then "Checkmate. " ++ show nextPlayer ++ " won."
      else "Stalemate."
    else do
      putStrLn $ show player ++ "'s turn."
      putStrLn "Enter your move:"
      input <- readMaybe <$> getLine
      case input of
        Just move -> do
          case M.lookup move nextLegalBoards of
            Just nextBoard -> gameLoop nextBoard nextPlayer
            Nothing        -> retryTurn' "Illegal move. Try again."
        Nothing -> retryTurn' "Invalid input. Example of a valid input: e2e4"
  where
    nextLegalBoards = getNextLegalBoards board player
    nextPlayer      = getNextPlayer player
    retryTurn'      = retryTurn board player

retryTurn :: Board -> Color -> String -> IO ()
retryTurn board player message = do
  putStrLn message
  gameLoop board player
