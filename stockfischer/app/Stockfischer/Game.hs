module Stockfischer.Game where

import Stockfischer.Rules
import Stockfischer.Board
import Text.Read
import qualified Data.Map as M

data PlayerType = Human | Computer

playGame :: PlayerType -> PlayerType -> Color -> Board -> IO ()
playGame whitePlayerType blackPlayerType player board = do
  putStrLn $ case boardPerspective of
    White -> show board
    Black -> reverse $ show board
  if M.null nextLegalBoards
    then putStrLn $ if isInCheck board player
      then "Checkmate. " ++ show nextPlayer ++ " won."
      else "Stalemate."
    else do
      putStrLn $ show player ++ "'s turn."
      case playerType of
        Computer -> do
          let (move, nextBoard) = head $ M.toList nextLegalBoards
          putStrLn $ "Computer plays: " ++ show move
          playNextTurn nextBoard
        Human -> do
          putStrLn "Enter your move:"
          input <- readMaybe <$> getLine
          case input of
            Just move -> do
              case M.lookup move nextLegalBoards of
                Just nextBoard -> playNextTurn nextBoard
                Nothing        -> retryTurn "Illegal move. Try again."
            Nothing -> retryTurn "Invalid input. Example of a valid input: e2e4"
  where
    nextLegalBoards   = getNextLegalBoards board player
    nextPlayer        = getNextPlayer player
    playTurn          = playGame whitePlayerType blackPlayerType
    playNextTurn      = playTurn nextPlayer
    retryTurn message = putStrLn message >> playTurn player board
    playerType        = case player of White -> whitePlayerType; Black -> blackPlayerType
    boardPerspective  = selectPerspective whitePlayerType blackPlayerType player

selectPerspective :: PlayerType -> PlayerType -> Color -> Color
selectPerspective Human    Human    player = player
selectPerspective Human    Computer _      = White
selectPerspective Computer Human    _      = Black
selectPerspective Computer Computer _      = White
