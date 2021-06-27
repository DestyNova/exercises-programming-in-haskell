module Ch9 where

-- 9.3
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x) = show x
  show (App op x y) = brak x ++ show op ++ brak y
    where brak (Val n) = show n
          brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val x) = [x]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val x) = [x | x > 0]
eval (App op l r) = [apply op x y | x <- eval l, y <- eval r, valid op x y]
