vectorLength :: ComplexNum->Float 
vectorLength (ToComplex real im) = sqrt (real*real + im* im)

data ComplexNum = ToComplex Float Float

instance Show ComplexNum where
    show (ToComplex real 0) = show real
    show (ToComplex 0 im) | im == 1 = "i"
                          | im == -1 = "-i"
    show (ToComplex real im) | im > 0  = if im > 1 then show real ++ "+" ++ show im ++ "i" else show real ++ "+" ++ "i" 
                             | im < 0 = if im < -1 then show real ++ show im ++ "i" else show real ++ "-i"
instance Eq ComplexNum where 
    (==) (ToComplex real1 im1) (ToComplex real2 im2) | real1 == real2 && im1 == im2 = True
                                                     | otherwise = False

instance Ord ComplexNum where 
    compare c1 c2 = compare (vectorLength c1) (vectorLength c2)

data QuantumState = ToQuantumState ComplexNum String

 