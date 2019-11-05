import Data.Char
import Data.List

charToInt :: Char->Int
charToInt x | ord x > 47 && ord x < 58 = ord x - 48
            | ord x > 96 && ord x < 123 = ord x - 87
            | ord x > 64 && ord x < 91 = ord x - 29
            | otherwise = error "incorrect letter"

toDecimal ::  Int -> String ->String
toDecimal snumber [] = error "number is empty :c"
toDecimal 1 base  = show $ foldr (\xs x -> x + 1) 0 base - 1 -- dunno whether its better to do charToInt right in mult  
toDecimal base snumber  | base > 61 || base < 1  = error "wrong base"
                        | otherwise = show $ foldl (\ys y -> charToInt y + base * ys) 0 snumber

intToChar :: Int->Char  
intToChar x | x >= 0 && x < 10 = chr (x + 48)
            | x >=10 && x < 36 = chr (x + 87)
            | x >= 36 && x < 62 = chr (x + 29)
            | otherwise = error "wrong input"

fromDecimal :: Int->String->String
fromDecimal 1 snumber = replicate (read snumber) '1'
fromDecimal toBase snumber = divide (read snumber) toBase snumber
                            where 
                                divide 0 toBase snumber = []
                                divide num toBase snumber | toBase < 0 || toBase > 61 = error "wrong input"
                                                          | otherwise =  divide (num `div` toBase) toBase snumber ++ (intToChar (num `mod` toBase) : []) -- НЕ КОСТЫЛЬ ЭТО! НЕ КОСТЫЛЬ!!!!

convertFromTo :: Int->Int->String->String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber
