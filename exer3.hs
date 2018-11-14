-- Martin Xaverius Jo / 301325516
-- Exercise 3 - CMPT 383 
-- Date: Monday, September 24 2018

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

-- Merging
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:as) (b:bs) 
  | a < b        = a : (merge as (b:bs))
  | otherwise    = b : (merge (a:as) bs)


-- Tail Recursive Hailstone
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

hailstone :: Int -> Int
hailstone n 
  | isEven(n) == True  = n `quot` 2
  | isEven(n) == False = 3 * n + 1

-- Hailstone length (Tail Recursion)
hailLen :: Int -> Int
hailLen n = hailTail 0 n
   where 
      hailTail a 1 = a
      hailTail a n = hailTail (a+1) (hailstone(n))

-- Factorial
fact :: Integer -> Integer
fact n = factorial 1 n
   where 
      factorial a 1 = a
      factorial a n = factorial (a*n) (n-1)

fact' :: Integer -> Integer
fact' n = foldl (*) 1 xs
   where xs = [1..n]

-- Haskell Library and Dates
daysInYear :: Integer -> [Day]
daysInYear y =  [jan1..dec31]
   where jan1    = fromGregorian y 1 1
         dec31   = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday d 
  | snd (mondayStartWeek d) == 5      = True
  | otherwise                         = False

-- Primes and Divisors (From Exercise 2)
divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

isPrime n = divisors n == []

-- End of Primes and Divisors (Ex 2)

isPrimeDay :: Day -> Bool
isPrimeDay greg 
  | isPrime (getDay gregDate) == True = True
  | otherwise                       = False
      where gregDate = (toGregorian greg) :: (Integer, Int, Int)

getDay (y,m,d) = d

primeFridays :: Integer -> [Day]
primeFridays y = filter isPrimeDay (filter isFriday (daysInYear y))

