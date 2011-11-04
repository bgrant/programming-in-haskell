-- Programming in Haskell Ch. 6 --
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

-- 6.1
myExp :: Integer -> Integer -> Integer
x `myExp` 0 = 1
x `myExp` (n + 1) = x * (x `myExp` n)

-- 6.3
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && (myAnd xs)

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ (myConcat xs)

myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate (n+1) x = x : (myReplicate n x)

nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth (_:xs) (n+1) = nth xs n

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) | x == y    = True
                | otherwise = myElem x ys

-- 6.4
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

-- 6.5
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge fstHalf sndHalf
    where fstHalf = msort (fst (halve xs))
          sndHalf = msort (snd (halve xs))

-- 6.6

--sum of list of numbers
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + sum xs

--take given number of elements from beginning of a list
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : (myTake (n-1) xs)

--last element of nonempty list
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs 
