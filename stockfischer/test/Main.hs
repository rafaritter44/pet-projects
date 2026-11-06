module Main (main) where

import Test.Hspec
import Stockfischer.Rules
import Stockfischer.Board
import Stockfischer.Moves
import Control.Monad (foldM)
import qualified Data.Map as M

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
