module Main where

import Stockfischer.Game

main :: IO ()
main = do
  putStrLn "Welcome to Stockfischer!"
  selectGameMode

selectGameMode :: IO ()
selectGameMode = do
  putStrLn "Select game mode:"
  putStrLn "    White        Black"
  putStrLn "[1] Human    vs. Human"
  putStrLn "[2] Human    vs. Computer"
  putStrLn "[3] Computer vs. Human"
  putStrLn "[4] Computer vs. Computer"
  putStrLn "[0] Exit"
  option <- getLine
  case option of
    "1" -> startGame (Human   , Human)    >> selectGameMode
    "2" -> startGame (Human   , Computer) >> selectGameMode
    "3" -> startGame (Computer, Human)    >> selectGameMode
    "4" -> startGame (Computer, Computer) >> selectGameMode
    "0" -> pure ()
    _   -> putStrLn "Invalid option. Please enter 1, 2, 3, 4, or 0." >> selectGameMode
