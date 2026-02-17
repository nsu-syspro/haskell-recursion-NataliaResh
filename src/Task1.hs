{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (filter, foldl, foldr, head, init, last, length, map, read, reverse, show, sum, tail)

-----------------------------------
--
-- Checks whether the last digit is a valid check digit
-- for the rest of the given number using Luhn algorithm
--
-- Usage example:
--
-- >>> validate 3456
-- False
-- >>> validate 34561
-- True
-- >>> validate 34562
-- False
-- >>> validate 17893729974
-- True
-- >>> validate 17893729971
-- False
-- >>> validate 18
-- True
-- >>> validate 7
-- False
-- >>> validate 0
-- False
-- >>> validate 1
-- False

validate :: Integer -> Bool
validate n
    | n <= 0    = False
    | otherwise = luhn (toDigits (withoutLastDigit n)) == lastDigit n

lastDigit :: Integer -> Int
lastDigit n = fromIntegral n `mod` 10

withoutLastDigit :: Integer -> Integer
withoutLastDigit n
    | n < 10    = 0
    | otherwise = n `div` 10

-----------------------------------
--
-- Computes check digit for given digits using Luhn algorithm
--
-- Usage example:
--
-- >>> luhn [3,4,5,6]
-- 1
-- >>> luhn [1,7,8,9,3,7,2,9,9,7]
-- 4

luhn :: [Int] -> Int
luhn xs = (10 - (sum (map normalize (doubleEveryOther (reverse xs))) `mod` 10)) `mod` 10

-----------------------------------
--
-- Produces list of digits for given positive number;
-- otherwise (for zero and negative numbers) returns empty list
--
-- Usage example:
--
-- >>> toDigits 3456
-- [3,4,5,6]
-- >>> toDigits 0
-- []
-- >>> toDigits (-123)
-- []
-- >>> toDigits 10455464576576
-- [1,0,4,5,5,4,6,4,5,7,6,5,7,6]

toDigits :: Integer -> [Int]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (withoutLastDigit n) ++ [lastDigit n]

-----------------------------------
--
-- Produces list in reverse order to the given one
--
-- Usage example:
--
-- >>> reverse "Hello"
-- "olleH"
-- >>> reverse [3,4,5,6]
-- [6,5,4,3]
-- >>> reverse [1]
-- [1]
--- >>> reverse []
-- []

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-----------------------------------
--
-- Doubles every other digit starting from first one
--
-- Usage example:
--
-- >>> doubleEveryOther [6,5,4,3]
-- [12,5,8,3]
-- >>> doubleEveryOther [1]
-- [2]
-- >>> doubleEveryOther [1, 2]
-- [2,2]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther []         = []
doubleEveryOther [x]        = [x * 2]
doubleEveryOther (x1:x2:xs) = x1 * 2 : x2 : doubleEveryOther xs

-----------------------------------
--
-- Normalizes given number to single digit by subtracting 9
-- if it is greater than or equal to 10
--
-- (Assumes inputs between 0 and 18)
--
-- Usage example:
--
-- >>> normalize 12
-- 3
-- >>> normalize 1
-- 1
-- >>> normalize 0
-- 0
-- >>> normalize 10
-- 1

normalize :: Int -> Int
normalize n
    | n < 10    = n
    | otherwise = n - 9

-----------------------------------
--
-- Produces list with given function applied to each element
-- in given list
--
-- Usage example:
--
-- >>> map (\x -> x * 2) [1,2,3,4]
-- [2,4,6,8]
--- >>> map (+ 1) [0, 1, 2]
-- [1,2,3]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f [x]    = [f x]
map f (x:xs) = f x : map f xs

-----------------------------------
--
-- Computes sum of given list of numbers
--
-- Usage example:
--
-- >>> sum [3,8,5,3]
-- 19
-- >>> sum []
-- 0

sum :: [Int] -> Int
sum []     = 0
sum [x]    = x
sum (x:xs) = x + sum xs
