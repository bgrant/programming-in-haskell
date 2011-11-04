-- Programming in Haskell Ch. 2 --
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
