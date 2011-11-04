-- Ch. 3 --

myAdd :: Int -> Int -> Int
myAdd a b = a + b

-- 3.1
-- :type ['a', 'b', 'c'] -- -> [Char]
-- :type ('a', 'b', 'c') -- -> (Char,Char,Char)
-- :type [(False, 'O'), (True , '1')] -- -> [(Bool, Char)]
-- :type ([False,True], ['0','1'])  -- -> ([Bool], [Char])
-- :type [tail, init, reverse ] -- -> [[a] -> [a]]

-- 3.2
second xs = head (tail xs) -- [a] -> a
swap (x,y) = (y,x) -- (a,b) -> (b,a)
pair x y = (x,y) -- a -> b -> (a,b)
double x = x * 2 -- (Num a) => a -> a
palindrome xs = reverse xs == xs -- (Eq a) => [a] -> Bool
twice f x = f (f x) -- (a -> a) -> a -> a
