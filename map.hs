mapl ::  (Integer -> Integer)->[Integer] -> [Integer]
mapr :: (Integer -> Integer)->[Integer] -> [Integer]
double :: Integer -> Integer
reversing :: [Integer]->[Integer]

double x = 2 * x 
reversing arr = foldl (\x y -> y:x) [] arr 
mapl f xs = reversing (foldl (\n y -> f y : n) [] xs)

mapr f xs = foldr (\x y -> f x : y) [] xs
{- из-за необходимисоти повторной итерации по массиву в случае 
foldl (при её отсуствии массив будет задом наперед) очевидно, что вариант foldr  
намного лучше
-}