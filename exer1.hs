-- Martin Xaverius Jo / 301325516
-- Exercise 1 - CMPT 383 
-- Date: Monday, September 10 2018
-- exer1.hs (done in gedit)

-- First Haskell Code
det a b c = b ^ 2 - 4 * a * c

quadsol1 a b c = (-b - sqrt (det a b c)) / 2 * a
quadsol2 a b c = (-b + sqrt (det a b c)) / 2 * a

-- Writing my First Code
third_a n = n !! 2
third_b (_:_:x:xs) = x

-- Hailstone Function
isEven n = n `mod` 2 == 0

hailstone n 
  | isEven(n) == True  = n `div` 2
  | isEven(n) == False = 3 * n + 1
