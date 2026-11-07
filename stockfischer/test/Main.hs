module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe, expectationFailure, Expectation)
import Stockfischer.Rules
  ( GameState(PlayerTurn, GameOver)
  , GameResult(Checkmate)
  , getGameState
  )
import Stockfischer.Board (Game, initialBoard)
import Stockfischer.Moves (Move)
import Control.Monad (foldM)
import qualified Data.Map as M (lookup)

main :: IO ()
main = hspec $ do
  describe "Stockfischer.Rules.getGameState" $ do
    it "delivers fool's mate" $ do
      let moves = map read ["f2f3", "e7e6", "g2g4", "d8h4"]
      shouldEndIn moves Checkmate

shouldEndIn :: [Move] -> GameResult -> Expectation
shouldEndIn moves result =
  case foldM playLegalMove [initialBoard] moves of
    Left message -> expectationFailure message
    Right game   -> getGameState game `shouldBe` GameOver result

playLegalMove :: Game -> Move -> Either String Game
playLegalMove game move = case getGameState game of
  PlayerTurn legalMoves -> case M.lookup move legalMoves of
    Just board -> Right (board:game)
    Nothing    -> Left $ "Unexpected illegal move: " ++ show move
  GameOver result -> Left $ "Game ended unexpectedly: " ++ show result
