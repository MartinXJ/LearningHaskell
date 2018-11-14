-- Martin Xaverius Jo / 301325516
-- Exercise 4 - CMPT 383 
-- Date: , October 3 2018

-- From previous exercise: Hailstone Function

import Data.Maybe

-- ----------------------------------------------------------------
-- From Exercise 3

isEven n = n `mod` 2 == 0

hailstone :: Integer -> Integer
hailstone n 
  | isEven(n) == True  = n `quot` 2
  | isEven(n) == False = 3 * n + 1

hailLen :: Integer -> Int
hailLen n = hailTail 0 n
   where 
      hailTail a 1 = a
      hailTail a n = hailTail (a+1) (hailstone(n))

-- Merge function
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs) 
  | a < b        = a : (merge as (b:bs))
  | otherwise    = b : (merge (a:as) bs)

-- ----------------------------------------------------------------

-- Exercise 4 (Starting point)

-- Hailstone, Again to find Hailstone Sequence (Recursive version)
hailSeq :: Integer -> [Integer]
hailSeq 1 = [1]
hailSeq n = n : hailSeq(hailstone n)

-- Non-recursive version of hailSeq
hailSeq' :: Integer -> [Integer]
hailSeq' n = (take length $ iterate hailstone n)
-- Need plus one since the hailLen calculate the length before it reaches one; 
-- however, we need the last bit when it reaches one.
   where
      length = hailLen n + 1

-- Joining Strings, Again
-- Using foldr or foldl, or foldl1 or foldr1
-- Not recursive
join :: [Char] -> [[Char]] -> [Char]
join sep [] = []
join sep (x:xs) = foldl op x xs
   where 
       op x xs = x ++ sep ++ xs

-- Merge Sort (unfinished)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = merge(mergeSort left) (mergeSort right)
    where
     (left, right) = splitAt ((length x) `div` 2) x

-- Monad
-- Searching? Maybe?
findElt ::  (Ord a, Eq a) => a -> [a] -> Maybe Integer
findElt matched xs = findEltIndex 0 xs
    where
          findEltIndex index [] = Nothing
          findEltIndex index (x:xs)
             | matched == x   = Just index  
             | otherwise      = findEltIndex (index + 1) xs





