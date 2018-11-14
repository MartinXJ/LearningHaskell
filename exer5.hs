-- Martin Xaverius Jo / 301325516
-- Exercise 5 - CMPT 383 
-- Date: Wednesday, October 10 2018

-- Built-in Functions (Recursive)
myIterate :: (a -> a) -> a -> [a] 
myIterate f x = x : myIterate f (f x)

myTakeWhile :: (a -> Bool) -> [a] -> [a] 
myTakeWhile f (x:xs)
  | f x == False   = []
  | otherwise      = x : myTakeWhile f xs

-- Pascal's Triangle
findPreviousRow :: [Integer] -> [Integer]
findPreviousRow xs = map (uncurry (+)) (zip xs (tail xs))

pascal :: Integer -> [Integer]
pascal 0 = [1]
pascal 1 = [1,1]
pascal n = combineMiddle (n)
   where combineMiddle n = [1] ++ findPreviousRow(pascal(n-1)) ++ [1]

-- Pointfree Addition
addPair :: (Integer, Integer) -> Integer
addPair = (uncurry (+))

-- Pointfree Filtering
withoutZeros :: (Eq a, Num a) => [a] -> [a]
withoutZeros = filter(/= 0)
  
-- Exploring Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- Infinite sequence of Fibonacci numbers
fibs :: [Integer]
fibs = map fib [0..]

-- Something Else..
things :: [Integer]
things = 0 : 1 : zipWith (+) things (tail things)

-- Refined Fibonacci
fibFast n = things !! n

