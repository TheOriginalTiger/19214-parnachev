import Data.Char
import Data.List

charToInt :: Char->Int
charToInt x | ord x > 47 && ord x < 58 = ord x - 48
            | ord x > 96 && ord x < 123 = ord x - 87
            | ord x > 64 && ord x < 91 = ord x - 29
            | otherwise = error "incorrect letter"

toDecimal ::  Int -> String ->Int
toDecimal snumber [] = error "number is empty :c"
toDecimal 1 base  = foldr (\xs x -> x + 1) 0 base - 1 -- dunno whether its better to do charToInt right in mult  
toDecimal base snumber = mult (foldr (\ys y ->y + 1) 0 snumber - 1) ( map charToInt snumber ) base
                            where
                                mult n [] num = 0
                                mult n (x:xs) num | base < 0 || base > 61 || x >= base = error "wrong input"
                                                  | otherwise = x * num ^ n + mult (n - 1) xs num

intToChar :: Int->Char  
intToChar x | x >= 0 && x < 10 = chr (x + 48)
            | x >=10 && x < 36 = chr (x + 87)
            | x >= 36 && x < 62 = chr (x + 29)
            | otherwise = error "wrong input"
fromDecimal :: Int->String->String
from toDecimal 1 0 = []
fromDecimal 1 snumber = turing (foldl (\x y -> x*10 + y) 0 (map charToInt snumber))
                                where
                                    turing 0 = ['1'] 
                                    turing number = '1' : turing (number - 1)
fromDecimal toBase snumber = reverse $ divide (foldl (\x y -> x*10 + y) 0 (map charToInt snumber)) toBase snumber
                            where 
                                divide 0 toBase snumber = []
                                divide num toBase snumber | toBase < 0 || toBase > 61 = error "wrong input"
                                                          | otherwise = intToChar (num `mod` toBase) : divide (num `div` toBase) toBase snumber

convertFromTo :: Int->Int->String->String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ show $ toDecimal fromBase snumber
