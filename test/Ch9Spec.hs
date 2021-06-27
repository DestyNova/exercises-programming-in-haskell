module Ch9Spec (spec) where

import Test.Hspec

-- ex1

-- tests
spec :: Spec
spec = do
  describe "ex1" $ do
    it "does nothing" $ 2+2 `shouldBe` 4
