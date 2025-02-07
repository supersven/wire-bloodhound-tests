module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "putMapping" $ do
    it "does something" $
      True `shouldBe` True
