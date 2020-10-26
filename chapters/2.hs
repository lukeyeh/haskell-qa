-- Write a function which multiplies a given number by ten. What is its type?

ten :: Num a => a -> a
ten x = x * 10

-- Write a function which returns True if both of its arguments are non-zero
-- and False otherwise. What is the type of your function?

nonzero :: (Eq a1, Eq a2, Num a1, Num a2) => a1 -> a2 -> Bool
nonzero x y = x /= 0 && y /= 0

-- Write a recursive functions sum' which, given a number n, calcualtes the sum
-- 1 + 2 + 3 + ... + n. What is its type?

sum' :: (Ord a, Num a) => a -> a
sum' n = if n < 1 then 0 else n + sum' (n - 1)

-- Write a function power x n which raises x to the power n. Give its type.

power :: (Ord a, Num a, Num b) => b -> a -> b
power x n = if n < 1 then 1 else x * power x (n - 1)

-- Write a function isConsonant which, given a lower-case character in the
-- range 'a'...'z', determines if it is a consonant.

isConsonant :: Char -> Bool
isConsonant c = 
  c /= 'a' && c /= 'e' && c /= 'i' && c /= 'o' && c /= 'u'
