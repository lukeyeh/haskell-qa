-- Write a function evenElements which does the opposite to oddElements,
-- returning the even numbered elements in a list. For example, evenElements
-- [2,4,2,4,2] should return [4,4]. What is the type of your function?

evenElements :: [a] -> [a]
evenElements [] = []
evenElements [_] = []
evenElements (_:x:xs) = x : evenElements xs

-- Write a function countTrue which counts the number of True elements in a
-- list. For example, countTrue [True, False, True] should return 2. What is
-- the type of your function?

countTrue :: (Num a) => [Bool] -> a
countTrue [] = 0
countTrue (x:xs) = if x then 1 + countTrue xs else countTrue xs

-- Write a function which, given a list, builds a palindrome from it. A
-- palindrome is a list which equals its own reverse. You can assume the
-- existance of reverse' and the list concatenation operator ++. Write another
-- function which determines if a list is a palindrome.

buildPalindrome :: [a] -> [a]
buildPalindrome [] = []
buildPalindrome [a] = [a]
buildPalindrome l = l ++ reverse l

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = (reverse l) == l

-- Write a function dropLast which returns all but the last element of a list.
-- If the list is empty, it should return the empty list. So, for example, 
-- dropLast [1,2,4,8] should return [1,2,4]

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

-- Write a function elem' of type Eq a => a -> [a] -> Bool which returns True
-- if an element exists in a list, or False if not. For example,
-- elem' 2 [1,2,3] should evaluate to True, but elem'3 [1,2] should evaluate
-- to False.

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = if x == a then True else elem' a xs

-- Use your elem' function to write a function makeSet which, given a list,
-- returns a list which contains all the elements of the original list, but has
-- no duplicate elements. For example, makeSet [1,2,3,3,1] might return
-- [2,3,1]. What is the type of your function?

makeSet :: (Eq a) => [a] -> [a]
makeSet [] = []
makeSet [x] = [x]
makeSet (x:xs) = if elem' x xs then makeSet xs else x : makeSet xs

-- Can you explain why the reverse' function we defined is inefficient? How
-- does the time it takes to run relate to the size of its argument? Can you
-- write a more efficient version using an extra argument?

revInner :: [a] -> [a] -> [a]
revInner acc [] = acc
revInner acc (x:xs) = revInner (x : acc) xs

-- What are the lists [1 .. 0] and [1, 2, .. 0]? How might we make the list
-- [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

a = [10,9..1]

-- Use a list comprehension to build a list of all positive numbers less than
-- 10,000 which have both 21 and 83 as factors, and the list which either as
-- factors. 

b = [x | x <- [1..10000], x `rem` 21 == 0, x `rem` 83 == 0]

-- Use a list comprehension to simplify your answer for question 2

countTrueList :: [Bool] -> Int
countTrueList l = length [x | x <- l, x]
