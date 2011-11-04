-- Programming in Haskell Ch. 5 --
--
-- :author: Robert David Grant <robert.david.grant@gmail.edu>
--
-- :copyright:
--   Copyright 2011 Robert David Grant
--
--   Licensed under the Apache License, Version 2.0 (the "License"); you
--   may not use this file except in compliance with the License.  You
--   may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
--   implied.  See the License for the specific language governing
--   permissions and limitations under the License.

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
