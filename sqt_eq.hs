func 0 b c = ( (-1) * c / b, (-1) * c / b)


func a b c  | d < 0 = error "no real roots"
            | otherwise = (x1, x2) 
            where 
                d = b * b - 4 * a * c
                x1 = (-b + sqrt d)/(2*a)
                x2 = (-b - sqrt d)/(2*a)