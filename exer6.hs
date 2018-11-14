-- Martin Xaverius Jo / 301325516
-- Exercise 6 - CMPT 383 
-- Finished on: Friday, October 19 2018

import Data.Ratio

-- Rational Numbers --
-- using list comprehension
rationalSum :: Integer -> [Ratio Integer]
rationalSum n = [x % y | x <- [1..n], y <- [1..n], x + y == n]

-- trying not to use list comprehension
-- rationalSum' :: Int -> [Ratio Int]
-- rationalSum' n = take (n-1) $ iterList 1 n
   -- where iterList a n = formRatio a (n-a) ++ iterList (a+1) n

formRatio a b = [a%b]

-- Lowest Terms Only
-- using list comprehension
rationalSumLowest :: Integer -> [Ratio Integer]
rationalSumLowest n = [x % y | x <- [1..n], y <- [1..n], x + y == n && (gcd x y) == 1]
 
-- All Rational numbers
rationals :: [Ratio Integer]
rationals = concat $ map rationalSumLowest [2..]

-- Input/Output
sumFile :: IO()
sumFile = do
  tableNums <- readFile "input.txt"
  let numLines = readTable tableNums 
  let sumFile = sum numLines
  print sumFile

-- Helper function
-- lines $ tName splits the string based on \n, creating the list of ["Int1", "Int2", "Int3" ...]
-- It separates the number based on \n
readTable :: String -> [Int]
readTable tName = map readEachLine(lines $ tName)
  where readEachLine xs = (read xs :: Int)



 
