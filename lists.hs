--1
myget :: [a] -> Integer -> a
myget [] n = error "no such element in list"
myget (x:xs) 0 = x 
myget (x:xs) n = myget xs (n-1)

--2
myhead :: [a] -> a
myhead [] = error "empty list"
myhead (x:xs) = x

--3
mylast :: [a] -> a
--myreverse
mylast xs = myhead (foldl (\x y ->  y:x) [] xs)

--4
mytail :: [a] -> [a]
mytail [] = error "empty list"
mytail (x:xs) = xs

--5
myinit :: [a] -> [a]
myinit [] = error "empty list"
myinit [x] = []
myinit (x:xs) = x: myinit xs

--6
myreverse :: [a]->[a]
myreverse [] = error "empty list"
myreverse xs = foldl (\x y -> y:x) [] xs

--7 
mylength :: [a] -> Integer
mylength xs = foldr (\x y -> y + 1) 0 xs 

--8 
myappend :: [a]->a->[a]
myappend [y] x = y : [x]
myappend (y:ys) x = y : myappend ys x

--9
myconcat :: [a]->[a]->[a]
myconcat [x] sys = x:sys
myconcat (x:xs) sys = x: myconcat xs sys

--10
-- match errors
mydrop :: Integer -> [a] -> [a]
mydrop 0 xs = xs
mydrop n [] = error "list index out of range"
mydrop n (x:xs) = mydrop (n-1) xs

--11
--errors
mytake :: Integer->[a]->[a]
mytake 0 xs = []
mytake n [] = error "list index out of range"
mytake n (x:xs) = x: mytake (n-1) xs

--12    
-- () instead 
-- make more faster
mysplit :: Integer->[a]->([a], [a])
mysplit n xs = (left n xs, right n xs) where -- also could use mytake & mydrop but its too boring 
                left 0 xs = [] -- monkeycoding!
                left n (l:ls) = l: left (n-1) ls --everywhere!
                right 0 rs = rs --hooray!
                right k (r:rs) = right (k-1) rs --I love it
--splitAt' :: Integer -> [a] -> ([a], [a])


--splitAt' n xs = ()

    
--13
mynull :: [a]->Bool
mynull [] = True
mynull xs = False

--14
myelem :: Eq a => [a]->a->Bool
myelem [] y = False
myelem (x:xs) y | x == y = True
                | otherwise = myelem xs y

--15
myfilter :: (a->Bool)->[a]->[a]
myfilter tst [] = []
myfilter tst (x:xs) = if tst x then x: (myfilter tst xs) else myfilter tst xs --duno how to do ir more declarative

--16
mymap :: (a -> b)->[a] -> [b]
mymap f xs = foldr (\x y -> f x : y) [] xs

--17
myzip :: [a]->[b]->[(a,b)]
myzip [] [] = []
myzip [] ys = error "lists are not equal"
myzip xs [] = error "lists are not equal"
myzip (x:xs) (y:ys) = (x,y):myzip xs ys

--18
myfoldl :: (a->b->b)->b->[a]->b
myfoldl f acc [] = acc
myfoldl f acc (x:xs) = myfoldl f (f x acc) xs

--19
myfoldr :: (a->b->b)->b->[a]->b
myfoldr f acc [] = acc
myfoldr f acc (x:xs) = x `f` myfoldr f acc xs
