func 0 b c = ( -c / b, -c / b)
func 0 0 c = error "no roots for const"

func a b c  | d < 0 = error "no real roots"
            | otherwise = (x1, x2) 
            where 
                d = b * b - 4 * a * c
                x1 = (-b + sqrt d)/(2*a)
                x2 = (-b - sqrt d)/(2*a)