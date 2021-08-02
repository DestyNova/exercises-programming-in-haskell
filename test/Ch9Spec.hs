module Ch9Spec (spec) where

import Ch9
import Test.Hspec

-- tests
spec :: Spec
spec = do
  describe "chapter 9.3" $ do
    it "validates Add" $ valid Add 1 2 `shouldBe` True
    it "rejects bad Sub" $ valid Sub 1 2 `shouldBe` False
    it "applies div" $ apply Div 9 3 `shouldBe` 3
    it "shows an expr" $ show (App Div (Val 6) (Val 2)) `shouldBe` "6/2"
    it "shows a big expr" $ show (App Div (Val 6) (App Add (Val 1) (Val 2))) `shouldBe` "6/(1+2)"
    it "extracts Vals" $ values (App Div (Val 6) (App Add (Val 1) (Val 2))) `shouldBe` [6,1,2]
    it "evals" $ eval (App Div (Val 6) (App Add (Val 1) (Val 2))) `shouldBe` [2]
    it "doesn't eval invalid" $ eval (App Div (Val 6) (App Add (Val 1) (Val 4))) `shouldBe` []
    it "solves a Countdown problem" $ eval (head (solutions [10,2,8,50,100] 155)) `shouldBe` [155]

  describe "chapter 9.11" $ do
    it "validates isChoice" $ isChoice [1,2,3] [1,2,3,4] `shouldBe` True
    it "rejects isChoice" $ isChoice [1,2,5] [1,2,3,4] `shouldBe` False
