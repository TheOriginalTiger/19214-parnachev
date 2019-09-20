func a b c  | a == 0 && b/= 0 = ( (-1) * c / b, (-1) * c / b)
            | a == 0 && b == 0 = error "no roots"
            | d < 0 = error "no real roots"
            | otherwise = (x1, x2) 
            where 
                d = b * b - 4 * a * c
                x1 = ((-1) * b + sqrt d)/(2*a)
                x2 = ((-1) * b - sqrt d)/(2*a)