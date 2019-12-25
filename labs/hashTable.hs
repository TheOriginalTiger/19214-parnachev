import Data.Char
import Data.List
import Data.Tuple
import System.IO
import System.Exit

type Capacity = Int
type Loadfactor = Double
type Elems = Int
 

data HashTable k v = ToHashTable [[(k,v)]] Capacity Loadfactor Elems deriving Show


enumerate :: [a]->[(Int,a)]
enumerate = zip [0..] 

hash:: (Show k ) => k -> Int
hash x = foldr (\x xs -> xs + ord x) 0 (show x)  

updateLF :: HashTable k v-> HashTable k v 
updateLF (ToHashTable table c l e) = ToHashTable table c ((fromIntegral e) / (fromIntegral c)) e
 
checkLF :: (Eq k, Show k) => HashTable k v-> HashTable k v
checkLF (ToHashTable table c l e) | l > 0.5 = fromList $ concat table
                                  | otherwise = ToHashTable table c l e

customHashTable :: Int -> HashTable k v
customHashTable c  = ToHashTable (replicate c []) c 0 0  



defaultHashTable:: HashTable k v
defaultHashTable  = ToHashTable [[],[],[],[]] 4 0 0

clear :: HashTable k v -> HashTable k v
clear (ToHashTable table c l e)  =  ToHashTable (replicate c []) c l e 

insertToTable::(Show k, Eq k)=>HashTable k v -> k -> v-> HashTable k v
insertToTable (ToHashTable table c l e) key value  = checkLF $ updateLF $ ToHashTable (map
                                ( \(i,o) -> if i == mod (hash key) c then ([el| el<-o, fst el /= key] ++ [(key,value)]) else o ) 
                                (enumerate table)) 
                                c l (e + 1) 

fromList::(Show k, Eq k) => [(k,v)]->HashTable k v
fromList list = foldr (\h hs-> insertToTable hs (fst h) (snd h)) (customHashTable $ length list * 2 + 1) list

-- Удаляет элементы по заданному ключу.
erase ::(Show k, Eq k)=>HashTable k v->k->HashTable k v
erase (ToHashTable table c l e) key = updateLF $ ToHashTable (map
                               ( \(i,o) -> if i == mod (hash key) c then [el| el<-o, fst el /= key] else o ) (enumerate table) )
                                 c l (e - 1)


getSamelyHashed :: (Show k, Eq k)=>HashTable k v ->k-> [(k,v)]
getSamelyHashed (ToHashTable table c l e) key = concat $ [ o | (i,o) <- enumerate table, i == mod (hash key) c]

-- Проверка наличия значения по заданному ключу.
contains::(Show k, Eq k)=>HashTable k v -> k -> Bool;
contains table key = elem key (map fst (getSamelyHashed table key) )

-- Возвращает значение по ключу.
at::(Show k, Eq k)=>HashTable k v -> k -> Maybe v
at table key | contains table key = Just $ head [v | (k,v) <- (getSamelyHashed table key), k == key ] 
             | otherwise = Nothing

size::(Show k, Eq k)=>HashTable k v -> Int;
size (ToHashTable _ _ _ e) = e

empty::(Show k, Eq k)=>HashTable k v -> Bool;
empty(ToHashTable _ _ _ e ) = e > 0


mmap :: [String]->[(Int, Int)]
mmap list = [ (read $ head l, read $ last l)  | l <-( map words list)] 

main :: IO()
main = withFile "input.txt" ReadMode (\h-> do
                                            filelines <- hGetContents h >>= return . mmap . lines
                                            let y = fromList filelines
                                            print y                                           
  )
