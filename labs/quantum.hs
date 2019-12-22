
data Complex a = ToComplex a a

instance (Num a, Show a, Ord a, Eq a) => Show (Complex a) where -- ugly in here, but pretty oustide!
    show (ToComplex real 0) = show real
    show (ToComplex 0 im) | im == 1 = "i"
                          | im == -1 = "-i"
                          | otherwise = show im ++ "i"

    show (ToComplex real im) | im > 0  = if im > 1 then show real ++ "+" ++ show im ++ "i" else show real ++ "+" ++ "i" 
                             | im < 0 = if im < -1 then show real ++ show im ++ "i" else show real ++ "-i"
instance (Num a, Show a, Ord a, Eq a) => Eq (Complex a) where 
    (==) (ToComplex real1 im1) (ToComplex real2 im2) = (abs real1) == (abs real2) && (abs im1) == (abs im2)

instance (Num a, Show a, Ord a, Eq a) => Ord (Complex a) where
    compare (ToComplex real1 im1) (ToComplex real2 im2) = compare (real1*real1 + im1*im1) (real2*real2 + im2*im2)  

instance (Num a, Show a, Ord a, Eq a, Floating a) => Num (Complex a) where
    (+) (ToComplex real1 im1) (ToComplex real2 im2) = ToComplex (real1 + real2) (im1 + im2)
    (*) (ToComplex real1 im1) (ToComplex real2 im2) = ToComplex (real1*real2 - im1 * im2) (real1*real2 + im1 * im2)
    abs (ToComplex real im) = ToComplex (sqrt $ real * real + im * im) 0
    negate (ToComplex real im) = ToComplex real (negate im)
    fromInteger int  = ToComplex (fromInteger int) 0  
    signum (ToComplex 0 im) = 0
    signum (ToComplex real im) = ToComplex (signum real) 0 -- actually csgn


data QuantumState a = ToQuantumState a String

instance (Show a, Ord a, Eq a) => Show (QuantumState a) where
    show (ToQuantumState cx state) = show cx ++ " state is : "++ state

instance (Show a, Ord a, Eq a) => Eq (QuantumState a) where
    (==) (ToQuantumState c1 s1) (ToQuantumState c2 s2) | c1 == c2 && s1 == s2 = True

instance (Show a, Ord a, Eq a) => Ord (QuantumState a) where
    compare (ToQuantumState c1 s1) (ToQuantumState c2 s2) = compare c1 c2

instance Functor QuantumState where
    fmap f (ToQuantumState complexnum string) = ToQuantumState (f complexnum)  string


type Qubit a = [QuantumState a]

toList :: Qubit (Complex a)-> [Complex a]
toList q = [complex | (ToQuantumState complex _) <- q]

toLabelList :: Qubit (Complex a) -> [String]
toLabelList q = [str | (ToQuantumState _ str) <- q]

fromList :: [Complex a]->[String]->Qubit (Complex a)
fromList complexArr strArr = [ (ToQuantumState complex str) | complex <- complexArr, str <- strArr ]

toPairList :: Qubit (Complex a)->[(Complex a,String)]
toPairList q =[(complex, str) | (ToQuantumState complex str) <-q ] 

fromPairList :: [(Complex a,String)] -> Qubit (Complex a)
fromPairList pairs = [ (ToQuantumState complex str) | (complex, str) <- pairs]

scalar :: (Num a) => Complex a->Complex a->Complex a
scalar (ToComplex r1 im1) (ToComplex r2 im2) = ToComplex (r1*r2 - im1*im2) (r1*r2 + im1*im2)

scalarProduct :: (Num a, Show a, Ord a, Eq a) => Qubit (Complex a) ->Qubit (Complex a) ->a
scalarProduct q1 q2 = foldr (\c r -> c + r) 0 [r1*r2 + i1*i2 | (ToQuantumState (ToComplex r1 i1) _) <- q1, (ToQuantumState (ToComplex r2 i2) _) <- q2]

entagle :: (Num a, Show a, Ord a, Eq a, Floating a ) => Qubit (Complex a) ->Qubit (Complex a) ->Qubit (Complex a)
entagle q1 q2 = [ ToQuantumState (c1 * c1) (str1 ++ str2) | (ToQuantumState c1 str1)<-q1 , (ToQuantumState c2 str2) <- q1]

x  = [(ToQuantumState (ToComplex 1 1) "rofl"),(ToQuantumState (ToComplex 3 3) "rofl"),(ToQuantumState (ToComplex 2 2) "rofl")]

toLList :: Qubit (Complex a) -> [Complex a]
toLList [] = []
toLList ((ToQuantumState comp str):xs) = comp:(toList xs)
