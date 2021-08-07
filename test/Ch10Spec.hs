module Ch10Spec (spec) where

import Ch10
import Test.Hspec

-- tests
spec :: Spec
spec = do
  describe "chapter 10.1" $ do
    it "does nothing" $ True `shouldBe` True
