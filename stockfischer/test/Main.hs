module Main (main) where

import Test.Hspec
import Stockfischer.Rules
import Stockfischer.Board

main :: IO ()
main = hspec $ do
  describe "Stockfischer.Rules.getGameState" $ do
    it "delivers fool's mate" $ do
      let moves = map read ["f2f3", "e7e6", "g2g4", "d8h4"]
          game  = reverse $ scanl playMove initialBoard moves
      getGameState game `shouldBe` GameOver Checkmate
