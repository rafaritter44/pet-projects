module Stockfischer.Game where

import Stockfischer.Rules
import Stockfischer.Board
import Stockfischer.Players
import Text.Read
import qualified Data.Map as M

startGame :: PlayerTypes -> IO ()
startGame = (`playGame` [initialBoard])

playGame :: PlayerTypes -> Game -> IO ()
playGame playerTypes game = do
  putStrLn $ case getBoardPerspective playerTypes player of
    White -> show board
    Black -> reverse $ show board
  case getGameState game of
    GameOver result -> putStrLn $ case result of
      Checkmate   -> "Checkmate. " ++ show nextPlayer ++ " won."
      Draw reason -> show reason ++ "."
    PlayerTurn legalMoves -> do
      putStrLn $ show player ++ "'s turn."
      case getPlayerType player playerTypes of
        Computer -> do
          let (move, nextBoard) = head $ M.toList legalMoves
          putStrLn $ "Computer plays: " ++ show move
          playNextTurn nextBoard
        Human -> do
          putStrLn "Enter your move:"
          input <- readMaybe <$> getLine
          case input of
            Just move -> do
              case M.lookup move legalMoves of
                Just nextBoard -> playNextTurn nextBoard
                Nothing        -> retryTurn "Illegal move. Try again."
            Nothing -> retryTurn "Invalid input. Example of a valid input: e2e4"
  where
    board             = getCurrentBoard game
    player            = getCurrentPlayer game
    nextPlayer        = getNextPlayer player
    playTurn          = playGame playerTypes
    playNextTurn      = playTurn . (:game)
    retryTurn message = putStrLn message >> playTurn game
