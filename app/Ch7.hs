module Ch7 where

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = lMap
  where lMap [] = []
        lMap (x:xs) = f x : rMap xs
        rMap [] = []
        rMap (x:xs) = g x : lMap xs

altMap' f g = zipWith ($) (cycle [f,g])

luhn xs = summed `mod` 10 == 0
  where doubleEveryOther = altMap id (\x -> if x > 4 then x*2 - 9 else x*2) (reverse xs)
        summed = sum doubleEveryOther

main = do
  print $ altMap' (+10) (+100) [0..4] == [10,101,12,103,14]
  print $ luhn [7,9,9,2,7,3,9,8,7,1,3]
  print $ not $ luhn [7,9,9,2,7,3,9,8,7,1,4]
