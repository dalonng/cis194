-- file: Homework 1


-- Exercise 1

lastDigit :: Integer -> Integer
lastDigit x = mod x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (dropLastDigit n) ++ [lastDigit n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = lastDigit n : toDigitsRev (dropLastDigit n)

-- Exercise 2

doubleRev :: [Integer] -> [Integer]
doubleRev [] = []
doubleRev (x:[]) = [x]
doubleRev (x:y:[]) = x:[y*2]
doubleRev (x:y:zs) = x: y*2 : doubleRev zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleRev (reverse x))

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x < 10 = x
  | otherwise = sumDigits (toDigits x)
sumDigits (x:zs) = (sumDigits [x]) + (sumDigits zs)

-- Exercise 4

validateNum :: Integer -> Integer
validateNum n = mod (sumDigits ( doubleEveryOther ( toDigits n))) 10

validate :: Integer -> Bool
validate n
  | validateNum n == 0 = True
  | otherwise = False

-- Exercise 5 Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 _ _ _ = []
hanoi n source dest temp = (hanoi (n-1) source temp dest) ++ [(source, dest)] ++ (hanoi (n-1) temp dest source)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _= []
hanoi4 1 s d t1 t2 = [(s, t1)]
hanoi4 2 s d t1 t2 = [(s, t1), (s,d), (t1, d)]
hanoi4 3 s d t1 t2 = [(s, t1), (s,t2), (s, d), (t2, d), (t1, d)]
hanoi4 n s d t1 t2 = (hanoi4 (n-2) s t1 d t2) ++ [(s, t2)] ++ [(s, d)] ++ [(t2,d)] ++ (hanoi4 (n-2) t1 d s t2)
