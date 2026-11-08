module Stockfischer.AISpec (spec) where

import Test.Hspec
  ( Spec
  , Expectation
  , describe
  , it
  , shouldBe
  , expectationFailure
  )
import Stockfischer.SpecUtils (playMoves, getLegalMoves)
import Stockfischer.AI (chooseMove)
import Stockfischer.Moves (Move)

spec :: Spec
spec = do
  describe "Stockfischer.AI.chooseMove" $ do
    it "chooses mate" $ do
      let moves = map read ["f2f3", "e7e6", "g2g4"]
          mate  = read "d8h4"
      shouldChooseMove moves mate

shouldChooseMove :: [Move] -> Move -> Expectation
shouldChooseMove moves move = case playMoves moves of
  Left message -> expectationFailure message
  Right game   -> fst <$> chooseMove game <$> getLegalMoves game `shouldBe` Right move
