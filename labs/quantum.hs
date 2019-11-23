
data Complex a = ToComplex a a

instance (Num a, Show a, Ord a, Eq a) => Show (Complex a) where -- ugly in here, but pretty oustide!
    show (ToComplex real 0) = show real
    show (ToComplex 0 im) | im == 1 = "i"
                          | im == -1 = "-i"
    show (ToComplex real im) | im > 0  = if im > 1 then show real ++ "+" ++ show im ++ "i" else show real ++ "+" ++ "i" 
                             | im < 0 = if im < -1 then show real ++ show im ++ "i" else show real ++ "-i"
instance (Num a, Show a, Ord a, Eq a) => Eq (Complex a) where 
    (==) (ToComplex real1 im1) (ToComplex real2 im2) | real1 == real2 && im1 == im2 = True
                                                     | otherwise = False
instance (Num a, Show a, Ord a, Eq a) => Ord (Complex a) where
    compare (ToComplex real1 im1) (ToComplex real2 im2) = compare (real1*real1 + im1*im1) (real2*real2 + im2*im2)  


data QuantumState a = ToQuantumState a String

instance (Show a, Ord a, Eq a) => Show (QuantumState a) where
    show (ToQuantumState cx state) = show cx ++ " state is : "++ state

instance (Show a, Ord a, Eq a) => Eq (QuantumState a) where
    (==) (ToQuantumState c1 s1) (ToQuantumState c2 s2) | c1 == c2 && s1 == s2 = True
                                                       | otherwise = False
instance (Show a, Ord a, Eq a) => Ord (QuantumState a) where
    compare (ToQuantumState c1 s1) (ToQuantumState c2 s2) = compare c1 c2

instance Functor QuantumState where
    fmap f (ToQuantumState complexnum string) = ToQuantumState (f complexnum)  string


type Qubit a = [QuantumState a]

toList :: Qubit (Complex a)-> [Complex a]
toList q = [complex | (ToQuantumState complex _) <- q]

toLabelList :: Qubit (Complex a) -> [String]
toLabelList q = [str | (ToQuantumState _ str) <- q]

fromList:: [Complex a]->[String]->Qubit (Complex a)
fromList complexArr strArr = [ (ToQuantumState complex str) | complex <- complexArr, str <- strArr ]

toPairList:: Qubit (Complex a)->[(Complex a,String)]
toPairList q =[(complex, str) | (ToQuantumState complex str) <-q ] 

fromPairList :: [(Complex a,String)] -> Qubit (Complex a)
fromPairList pairs = [ (ToQuantumState complex str) | (complex, str) <- pairs]
