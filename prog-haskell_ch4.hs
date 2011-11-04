-- Ch. 4 --

myAbs ::  Int -> Int
myAbs x = if x < 0 then -x else x

mySignum :: Int -> Int
mySignum x = if x < 0 then -1 else
                if x > 0 then 1 else
                    0

myAbs2 :: Int -> Int
myAbs2 x | x < 0     = -x
         | otherwise =  x

mySignum2 :: Int -> Int
mySignum2 x | x <  0 = -1
            | x == 0 =  0
            | x >  0 =  1

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myPred :: Int -> Int
myPred 0 = 0
myPred (n+1) = n

-- 4.1
halve1 :: [a] -> ([a], [a])
halve1 xs = splitAt (length xs `div` 2) xs

-- 4.2
safetail_a :: [a] -> [a]
safetail_a xs = if null xs then [] else tail xs

safetail_b :: [a] -> [a]
safetail_b xs | null xs   = []
              | otherwise = tail xs

safetail_c :: [a] -> [a]
safetail_c (_:xs) = xs
safetail_c _ = []

-- 4.3
va :: Bool -> Bool -> Bool
False `va` False = False
False `va` True  = True
True `va` False  = True
True `va` True   = True

vb :: Bool -> Bool -> Bool
False `vb` False = False
_ `vb` _ = True

vc :: Bool -> Bool -> Bool
False `vc` b = b
True `vc` _ = True

vd :: Bool -> Bool -> Bool
b `vd` c | b == c       = b
         | otherwise    = True

-- 4.4
myAnd2 :: Bool -> Bool -> Bool
myAnd2 x y = if (x == True) && (y == True) then True else False

-- 4.5
myAnd3 :: Bool -> Bool -> Bool
myAnd3 x y = if (x == True) then y else
                if (x == False) then False else False

-- 4.6
myMult1 :: Num a => a -> a -> a -> a
myMult1 x y z = x * y * z

myMult2 :: Num a => a -> a -> a -> a
myMult2 = \x -> \y -> (\z -> x * y * z)
