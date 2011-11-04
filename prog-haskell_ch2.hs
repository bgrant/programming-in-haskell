-- Ch. 2 --

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
    where
        b = 1
        c = 2
d = a * 2

-- 2.3
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- 2.4
myLast1 xs = head (drop ((length xs) - 1) xs)
myLast2 xs = head (reverse xs)
myLast3 xs = xs !! ((length xs) - 1)

-- 2.5
myInit1 xs = take ((length xs) - 1) xs
myInit2 xs = reverse (tail (reverse xs))
myInit3 xs = reverse (drop 1 (reverse xs))
