module Ch8Spec (spec) where

import Test.Hspec

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

-- ex3
balanced (Leaf _) = True
balanced (Node l r) = abs (lCount - rCount) <= 1 && balanced l && balanced r
  where lCount = leafCount l
        rCount = leafCount r

leafCount (Leaf _) = 1
leafCount (Node l r) = leafCount l + leafCount r

-- ex4
balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
  where l = take n xs
        r = drop n xs
        n = length xs `div` 2

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
