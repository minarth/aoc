module Main where

import Data.Char ()

-- DAY 4
x = [359282..820401]

getElem t n
  | n == 0 = head t
  | otherwise = getElem (tail t) (n-1)

generatePossible start end nums
  | start > end = nums
  | otherwise = generatePossible (start+1) end $ nums ++ [start]

toDigits :: Integer -> [Integer]
toDigits n 
  | n < 1 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
 
--hasConsecutive :: [Integer] -> Bool
hasConsecutive number
  | (length number) <= 1 = False
  | (number !! 0) == (number !! 1) = True
  | otherwise = hasConsecutive (tail number)

-- usage of consecutive filter
--filter hasConsecutive (map toDigits [1000..1200])

neverDecreases number
  | (length number) <= 1 = True
  | (number !! 0) > (number !! 1) = False
  | otherwise = neverDecreases (tail number)

-- usage
-- filter neverDecreases (map toDigits [1000..1500])

xDigits = map toDigits x

x1 = filter hasConsecutive xDigits
x2 = filter neverDecreases x1

--filter neverDecreases $ filter hasConsecutive $ map toDigits [111111]

-- PART TWO
hasConsecutive2 number
  | (length number) <= 2 = False
  | and [(length number) == 6, (number !! 3) /= (number !! 4), (number !! 4) == (number !! 5)] = True
  | and [(length number) == 6, (number !! 0) == (number !! 1), (number !! 2) /= (number !! 1)] = True
  | and [(length number) >= 4, (number !! 0) /= (number !! 1), (number !! 2) == (number !! 1), 
  (number !! 2) /= (number !! 3)] = True
  | otherwise = hasConsecutive2 (tail number)

x3 = filter hasConsecutive2 x2
-- !! 366667
-- !! 112222

xTest = filter hasConsecutive2 $ map toDigits [366667, 112222]

main = print(1+1)