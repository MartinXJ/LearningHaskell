-- Martin Xaverius Jo
-- 301325516
-- CMPT 383 - Assignment 1: Rainbow Tables with Haskell
-- Finished on: Friday, October 19 2018

-- ###  Run Steps
-- How to run:
-- 1. ghc -O2 --make -Wall rainbowtable.hs
-- 2. ./rainbowtable

import RainbowAssign
import Data.Maybe as Maybe
import qualified Data.Map as Map

-- Given parameters
-- Parameters can be changed
pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width    = 40           -- length of each chain in the table
height   = 1000         -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

-- ### More information on test numbers
-- The value currently is 1000, but if you want to change the test numbers. You change the value 1000 to any value.

-- Compiling the Code
main :: IO ()
main = do
 generateTable
 res <- test2 1000
 print res

-- Hashing and Reducing --

-- The input of pwReduce is the Hash value (From example is base-10 integer)
-- First, using fromEnum to convert the hashValue to Int. From Hash type (Int32) to Int
-- Second, it will generate an infinite sequence of the modulo by recursively dividing by nLetters, until it reaches pwLength
-- Third, you need to convert the sequence using toLetter, function from RainbowAssign
-- Fourth, reverse the list because we are reading from right to left or bottom to top
pwReduce :: RainbowAssign.Hash -> RainbowAssign.Passwd
pwReduce hashValue = reverse(map toLetter(take pwLength(hashResult (fromEnum hashValue))))
-- hashResult takes an integer type Int, keeps dividing and creating an infinite sequence of eventually zeros, and take the pwLength's length of the sequence 
       where hashResult n = [n `mod` nLetters] ++ hashResult (n `div` nLetters)

-- Tryouts, testing out what fromEnum does 
-- buildEnum :: RainbowAssign.Hash -> Int
-- buildEnum hashValue = fromEnum hashValue

-- The Map Data Structure --
-- Helper function to create a rainbow table. Map listPassword to a 
mapping :: Int -> [RainbowAssign.Passwd] -> [RainbowAssign.Hash]
mapping 0 listPasswd = map pwHash listPasswd
mapping n listPasswd = mapping (n-1) (hashReduce listPasswd)
    where hashReduce listPwd = map pwReduce (map pwHash listPwd)

-- Building the Table --
rainbowTable :: Int -> [RainbowAssign.Passwd] -> Map.Map Hash Passwd
rainbowTable tWidth listPasswd = Map.fromList(zip (mapping tWidth listPasswd) listPasswd)

-- Creating, Reading, and Writing Tables --

-- two functions given
generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename

-- the function test1 will not be used
-- test1 :: IO (Maybe RainbowAssign.Passwd)
-- test1 = do
   -- table <- readTable filename
   -- return (Map.lookup 777367630 table)

-- Reversing Hashes --

-- Helper functions --

-- This is to reduce the hashed Password.
hashReducePwd :: RainbowAssign.Passwd -> RainbowAssign.Passwd
hashReducePwd n = pwReduce (pwHash n)

-- Combine the next Hash, Passwd to create a new tuple. This is to keep searching whether the tuple equals to hash value.
combineHashPasswd :: RainbowAssign.Hash -> RainbowAssign.Passwd -> (RainbowAssign.Hash, RainbowAssign.Passwd)
combineHashPasswd hash newPasswd = (hash, newPasswd)

-- Self explanatory from function name
checkEveryRow :: (RainbowAssign.Hash, RainbowAssign.Passwd) -> Int -> RainbowAssign.Hash -> (RainbowAssign.Hash, RainbowAssign.Passwd)
checkEveryRow x 0 _                     = x

checkEveryRow x tWidth hashValue
    | (pwHash (snd (x))) == hashValue   = x  
    | otherwise                         = checkEveryRow (combineHashPasswd (fst x) (hashReducePwd (snd (x)))) (tWidth-1) hashValue

-- Recursively searching password from the table 
findPwdInTable :: [(RainbowAssign.Hash, RainbowAssign.Passwd)] -> Int -> RainbowAssign.Hash -> Maybe RainbowAssign.Passwd
findPwdInTable [] _ _                                            = Nothing

findPwdInTable (x:xs) tWidth hashValue 
-- case when it finds 
  | tWidth == 0 && (pwHash (snd x)) == hashValue                 = Just ((snd x))
  | (pwHash(snd(checkEveryRow x tWidth hashValue))) == hashValue = Just (snd (checkEveryRow x tWidth hashValue)) 
  | otherwise                                                    = findPwdInTable xs tWidth hashValue
 

findPassword :: Map.Map Hash Passwd -> Int -> RainbowAssign.Hash -> Maybe RainbowAssign.Passwd
findPassword listPasswd tWidth hashValue = findPwdInTable (Map.toList(listPasswd)) tWidth hashValue

-- Experimenting
test2 :: Int -> IO ([RainbowAssign.Passwd], Int)
test2 n = do
 table <- readTable filename
 pws   <- randomPasswords nLetters pwLength n
 let hs     = map pwHash pws
 let result = Maybe.mapMaybe (findPassword table width) hs
 return (result, length result)



