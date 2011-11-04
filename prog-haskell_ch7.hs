-- Programming in Haskell Ch. 7 --
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

-- 7.1
-- Reimplement [f x | x <- xs, p x]
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

mapFilter2 f p = map f . filter p

-- 7.2
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs) = (f x) && (myAll f xs)

myAll2 :: (a -> Bool) -> [a] -> Bool
myAll2 f = foldr ((&&) . f) True

--any
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False 
myAny f (x:xs) = (f x) || (myAny f xs)

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f = foldr ((||) . f) False

--takeWhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []                  = []
myTakeWhile f (x:xs) | f x        = x:(myTakeWhile f xs)
                     | otherwise  = []

--dropWhile
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ []                 = []
myDropWhile f (x:xs) | f x       = myDropWhile f xs
                     | otherwise = xs

-- 7.3
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr g []
    where g = (\x -> (\y -> ((f x):y)))

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr g []
    where g = (\x -> (\y -> if (f x) 
                            then (x:y)
                            else y))

-- 7.4
dec2int :: [Int] -> Int
dec2int xs = foldl f 0 xs
    where f = (\x y -> 10*x + y)

-- (10* 2 + 3) * 10 + 4) * 10 + 5

-- 7.5
-- sum . map (^2) . filter even
sse = sum . map (^2) . filter even

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

--f = compose [sum, map (^2), filter even]
-- definition given is invalid because a list has to contain items of the same
-- type, and this list would be
-- [([a] -> a), ([a] -> [a]), ([a] -> [a])]

-- 7.6
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = \x y -> f (x,y)

myUncurry:: (a -> b -> c) -> (a, b) -> c
myUncurry f = \xy -> f (fst xy) (snd xy)

pairPlus = myUncurry (+)
regPlus = myCurry pairPlus

-- 7.7
-- unfold
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (==0) (`mod` 2) (`div` 2)
-- x=13: 1 : 6
--       0 : 3
--       1 : 1
--       1 : 0

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

myMap7 :: (a -> b) -> [a] -> [b]
myMap7 f = unfold null (f . head) (drop 1)

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (\x -> False) id f

-- 7.8
bin2int	::	[Bit] -> Int 
bin2int = foldr (\x y -> x + 2 * y) 0

make8 :: [Bit] -> [Bit] 
make8 bits = take 8 (bits ++ repeat 0)

--encode :: String -> [Bit]
--encode = concat . map (make8 . int2bin . ord)

-- don't have ord... skipping this one
