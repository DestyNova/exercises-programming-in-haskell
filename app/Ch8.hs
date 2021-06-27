module Ch8 where

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

-- ex5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Add x y) = g (folde f g x) (folde f g y)
folde f _ (Val x) = f x

-- ex6
eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y

size :: Expr -> Int
size (Val _) = 1
size (Add x y) = size x + size y

-- ex7
-- -- not sure how to test these since they clash with the Prelude definitions
-- instance Eq a => Eq (Maybe a) where
--   Nothing == Nothing = True
--   Just x == Just y = x == y
--   _ == _ = False
--
-- instance Eq a => Eq [a] where
--   [] == [] = True
--   (x:xs) == (y:ys) = x == y && xs == ys
--   _ == _ = False
