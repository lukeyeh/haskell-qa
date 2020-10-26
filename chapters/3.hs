import Data.Char

-- Rewrite the not' function from the previous chapter in pattern matching
-- style

not' :: Bool -> Bool

not' True = False
not' False = True

-- Use pattern matching to write a recursive function sumMatch which, given a
-- positive integer n, returns the sum of all the integers from 1 to n

sumMatch :: (Eq a, Num a) => a -> a

sumMatch 0 = 0
sumMatch n = n + sumMatch (n - 1)

sumGuarded :: (Ord a, Num a) => a -> a

sumGuarded n | n < 1 = 0
             | otherwise = n + sumGuarded (n - 1)

-- Use pattern matching to write a function which, given two numbers x and n,
-- computes x^n

powMatch :: (Eq b, Num b, Num a) => a -> b -> a

powMatch _ 0 = 1
powMatch x n = x * powMatch x (n - 1)


powGuarded :: (Ord b, Num b, Num a) => a -> b -> a

powGuarded x n | n < 1     = 1
               | otherwise = x * powMatch x (n - 1)

-- use guarded equations to write a function which categorises characters into
-- 3 kinds: kind 0 for the lowercase letters a...z, kind 1 for the uppercase
-- letters a...z, and kind 2 for everything else

categoriseThree :: (Num a, Eq a) => Char -> a

categoriseThree c | isLower c = 0
                  | isUpper c = 1
                  | otherwise = 2
