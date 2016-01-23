-- CIS 194, Spring 2015

{-# OPTION_GHC -Wall #-}

module HW01 where

-- Exercise 1

lastDigit :: Integer -> Integer
lastDigit n = mod n 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- Exercise 2

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = (lastDigit n) : (toRevDigits (dropLastDigit n))

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toRevDigits n

-- Exercise 3

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x: (y+y):(doubleEveryOther xs)

-- Exercise 4

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum $ (toDigits x)) + (sumDigits xs)

-- Exercise 5

validValue = sumDigits . doubleEveryOther . toRevDigits

luhn :: Integer -> Bool
luhn n = (mod (validValue n) 10) == 0

-- Exercise 6

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 s d t = [(s, d)]
hanoi 2 s d t = [(s, t), (s, d), (t, d)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) b c a)


