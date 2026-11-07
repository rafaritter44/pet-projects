module Main (main) where

import Test.Hspec
  ( hspec
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  , expectationFailure
  , Expectation
  )
import Stockfischer.Rules
  ( GameState(PlayerTurn, GameOver)
  , GameResult(Checkmate, Draw)
  , DrawReason(ThreefoldRepetition)
  , getGameState
  )
import Stockfischer.Board (Game, initialBoard)
import Stockfischer.Moves (Move)
import Control.Monad (foldM)
import qualified Data.Map as M (lookup, member)

main :: IO ()
main = hspec $ do
  describe "Stockfischer.Rules.getGameState" $ do
    it "delivers mate" $ do
      let moves = map read ["f2f3", "e7e6", "g2g4", "d8h4"]
      shouldEndIn moves Checkmate

    it "draws" $ do
      let moves = map read $ concat $ replicate 2 ["g1f3", "g8f6", "f3g1", "f6g8"]
      shouldEndIn moves $ Draw ThreefoldRepetition

    it "can promote pawn" $ do
      let moves = map read
            [ "a2a4", "b7b5"
            , "a4b5", "a7a6"
            , "b5a6", "b8c6"
            , "a6a7", "c6b8"
            ]
          promotion = read "a7b8q"
      shouldAllowMove moves promotion

    it "can castle" $ do
      let moves = map read
            [ "e2e4", "e7e5"
            , "g1f3", "b8c6"
            , "f1c4", "g8f6"
            ]
          castling = read "e1g1"
      shouldAllowMove moves castling

    it "can capture en passant" $ do
      let moves     = map read ["e2e4", "e7e6", "e4e5", "d7d5"]
          enPassant = read "e5d6"
      shouldAllowMove moves enPassant

shouldEndIn :: [Move] -> GameResult -> Expectation
shouldEndIn moves result =
  case foldM playLegalMove [initialBoard] moves of
    Left message -> expectationFailure message
    Right game   -> getGameState game `shouldBe` GameOver result

shouldAllowMove :: [Move] -> Move -> Expectation
shouldAllowMove moves move =
  case foldM playLegalMove [initialBoard] moves of
    Left message -> expectationFailure message
    Right game   -> case getGameState game of
      PlayerTurn legalMoves -> legalMoves `shouldSatisfy` M.member move
      GameOver result       -> expectationFailure $ "Game ended unexpectedly: " ++ show result

playLegalMove :: Game -> Move -> Either String Game
playLegalMove game move = case getGameState game of
  PlayerTurn legalMoves -> case M.lookup move legalMoves of
    Just board -> Right (board:game)
    Nothing    -> Left $ "Unexpected illegal move: " ++ show move
  GameOver result -> Left $ "Game ended unexpectedly: " ++ show result
