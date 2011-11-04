-- Programming in Haskell Ch. 3 --
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
