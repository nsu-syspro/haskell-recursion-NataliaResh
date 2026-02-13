{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (filter, foldl, foldr, head, init, last, length, map, read, reverse, show, sum, tail)

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (map, reverse, sum, doubleEveryOther, toDigits)
import Text.Printf (IsChar(fromChar))

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN n mapping xs = (n - (sum (map (normalizeModN n) (doubleEveryOther (reverse (map mapping xs)))) `mod` n)) `mod` n

normalizeModN :: Int -> Int -> Int
normalizeModN n x
    | x < n    = x
    | otherwise = x - (n - 1)
-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15
-- >>> luhnHex 

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt c
    | inBound c '0' '9' = fromEnum c - fromEnum '0'
    | inBound c 'a' 'f' = fromEnum c - fromEnum 'a' + 10
    | inBound c 'A' 'F' = fromEnum c - fromEnum 'A' + 10
    | otherwise          = 0

inBound :: Ord a => a -> a -> a -> Bool
inBound x a b = a <= x && x <= b


validateModN :: Int -> (a -> Int) -> [a] -> Bool
validateModN n mapping xs = luhnModN n mapping (withoutLast xs) == mapping (last xs)

last :: [a] -> a
last [] = error "Empty list"
last [x] = x
last (_:xs) = last xs

-- >>> withoutLast "123abcf"
-- "123abc"

withoutLast :: [a] -> [a]
withoutLast [] = []
withoutLast [_] = []
withoutLast (x:xs) = x : withoutLast xs

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec x = validateModN 10 id (toDigits x)

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex = validateModN 16 digitToInt
