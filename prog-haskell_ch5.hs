-- Ch. 5 --

-- 5.1
sumSquares :: Integer
sumSquares = sum [x^2 | x <- [1..100]]

-- 5.2
myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1..n]]

-- 5.3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- xs, y <- xs, z <- xs, x^2 + y^2 == z^2]
            where xs = [1..n]

-- 5.4
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (init (factors x))]

-- 5.5
comp1 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
comp2 = concat [[(x,y) | x <- [1,2,3]] | y <- [4,5,6]]

-- 5.6
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
                    where n = length xs - 1

-- 5.7
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- (zip xs ys)]
