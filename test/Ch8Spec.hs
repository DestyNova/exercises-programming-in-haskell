module Ch8Spec (spec) where

import Ch8
import Test.Hspec

-- tests
spec :: Spec
spec = do
  describe "ex3" $ do
    it "balances trivially" $ balanced (Leaf 1) `shouldBe` True
    it "balances trivially 2" $ balanced (Node (Leaf 1) (Leaf 1)) `shouldBe` True
    it "balances trivially 3" $
      balanced (Node (Leaf 1) (Node (Leaf 1) (Leaf 1))) `shouldBe` True
    it "doesn't balance" $
      balanced (Node (Leaf 1) (Node (Node (Leaf 1) (Leaf 2)) (Leaf 1))) `shouldBe` False
  describe "ex4" $ do
    it "balances a singleton list" $ balance "x" `shouldBe` Leaf 'x'
    it "balances a 2-elem list" $ balance "xy" `shouldBe` Node (Leaf 'x') (Leaf 'y')
    it "balances a 5-elem list" $ balance "hello" `shouldBe`
      Node (Node (Leaf 'h') (Leaf 'e')) (Node (Leaf 'l') (Node (Leaf 'l') (Leaf 'o')))
    it "should produce balanced trees" $ balanced (balance "hello dude") `shouldBe` True
  describe "ex5" $
    it "foldes" $ let expr = Add (Val 3) (Add (Val 0) (Val 2))
                      in (folde (*2) (+) expr `shouldBe` 10)
  describe "ex6" $ do
    it "evals" $ let expr = Add (Val 3) (Add (Val 0) (Val 2))
                     in (eval expr `shouldBe` 5)
    it "sizes" $ let expr = Add (Val 3) (Add (Val 0) (Val 2))
                     in (size expr `shouldBe` 3)
