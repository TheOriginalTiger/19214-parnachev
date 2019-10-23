import Data.Char
import Data.List

charToInt :: Char->Int
charToInt x | ord x > 47 && ord x < 58 = ord x - 48
            | ord x > 96 && ord x < 123 = ord x - 87
            | ord x > 64 && ord x < 91 = ord x - 29
            | otherwise = error "incorrect letter"

toDecimal :: String -> Int -> Int
toDecimal [] snumber = error "number is empty :c"
toDecimal base 1 = foldr (\xs x -> x + 1) 0 base - 1 -- dunno whether its better to do charToInt right in mult  
toDecimal base snumber = mult (foldr (\ys y ->y + 1) 0 base - 1) ( map charToInt base ) snumber
                            where
                                mult n [] num = 0
                                mult n (x:xs) num | snumber < 0 || snumber > 61 || x >= snumber = error "wrong input"
                                                  | otherwise = x * num ^ n + mult (n - 1) xs num

intToChar :: Int->Char
intToChar x | x >= 0 && x < 10 = chr (x + 48)
            | x >=10 && x < 36 = chr (x + 87)
            | x >= 36 && x < 62 = chr (x + 29)
            | otherwise = error "wrong input"
fromDecimal :: String->Int->String
fromDecimal toBase snumber = "kek"