-- Write a simple recursive function calm to replace explanation marks in a
-- string with periods. For example calm "Help! Fire!" should evaluate to
-- "Help. Fire.". Now rewrite your function to use map' instead of recursion
-- What are the types of your functions?
replaceExclamationRecursive :: [Char] -> [Char] 
replaceExclamationRecursive [] = []
replaceExclamationRecursive (x:xs)
    | x == '!' = '.' : replaceExclamationRecursive xs
    | otherwise = x : replaceExclamationRecursive xs

replaceExclamationMap :: [Char] -> [Char]
replaceExclamationMap l = map replaceExclamation l
    where
        replaceExclamation c
            | c == '!' = '.'
            | otherwise = c 

-- Write a function clip which, given a number, clips it to the range 1..10 so
-- that numbers bigger than 10 round down to 10 and those smaller than 1 round
-- up to 1. Write another function clipList which uses this first function
-- together with map' to apply this clipping to a whole list of numbers
clipList :: (Ord a, Num a) => [a] -> [a]
clipList l = map clip l 
    where
        clip x 
            | x > 10 = 10
            | x < 1 = 1
            | otherwise = x

-- Write a function apply which, given another function, a number of times to
-- apply it, and an initial argument for the function, will return the 
-- cumulative effect of repeatedly applying the function. For instance, 
-- apply f 6 4 should return f ( f ( f (f (f (f 4))))). What is the type of your
-- function?
compositeFuncN :: (Eq b, Num b) => (a -> a) -> b -> a -> a
compositeFuncN f 1 x = f x
compositeFuncN f n x = f (compositeFuncN f (n - 1) x)

-- Modify the insertion sort function from the preceding chapter to take a 
-- comparison function, in the same way that we modified merge sort in this
-- chapter. What is the type?
insertionSort' :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
insertionSort' _ [] = []
insertionSort' _ [x] = [x]
insertionSort' cmp (x:xs) = insertComplete x (insertionSort' cmp xs)
  where
    insertComplete a [] = [a]
    insertComplete a (b:bs) =
      if cmp a b 
        then a : b : bs
        else b : insertComplete a bs

-- Write a function filter' which takes a function of type a -> Bool and an [a]
-- and returns a list of just those elements of the argument list for which
-- the given function returns True
filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []
filter' f (x:xs) 
    | f x = filter' f xs
    | otherwise = x : filter' f xs 

-- Write the function all' which, given a function of type a -> Bool an an
-- argument list of type [a] evaluates to True if and only if the function
-- returns True for every element of the list. Give examples of its use.
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs)
    | f x = all' f xs
    | otherwise = False

-- Write a function mapl which maps a function of the type a -> b over a list of
-- type [[a]] to produce a list of type [[b]]
mapList' :: (a -> b) -> [[a]] -> [[b]]
mapList' f l = map (map f) l

mapList'' :: (a -> b) -> [[a]] -> [[b]]
mapList'' f = map (map f)

mapList''' :: (a -> b) -> [[a]] -> [[b]]
mapList''' = map . map 




