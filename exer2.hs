-- Martin Xaverius Jo / 301325516
-- Exercise 2 - CMPT 383 
-- Date: Monday, September 17 2018

-- From previous exercise: Hailstone Function
isEven n = n `mod` 2 == 0

hailstone n 
  | isEven(n) == True  = n `quot` 2
  | isEven(n) == False = 3 * n + 1

-- Hailstone length
hailLen 1 = 0
hailLen x = 1 + hailLen(hailstone x)

-- Primes and Divisors
divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]

-- Joining strings
join _ [] = []
join _ [x] = x
join sep (x:xs) = x ++ sep ++ join sep xs
 
-- Pythagorean Triples
pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(a,b,c) | c <- [2..n], b <- [2..n], a <- [2..n], (isTriple a b c) && (a < b)]

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = a^2 + b^2 == c^2
