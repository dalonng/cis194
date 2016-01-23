module Golf where

import qualified Data.List as L

-- Exercise 1 Hopscotch

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n xs
  | n <= length xs = (last $ take n xs):(everyNth n $ drop n xs)
  | otherwise = []

uuu :: Int -> [a] -> [[a]]
uuu 0 _ = []
uuu n [] = []
uuu n xs = (uuu (n-1) xs) ++ [(everyNth n xs)]

skips :: [a] -> [[a]]
skips [] = []
skips xs = uuu (length xs) xs

-- Exercise 2 Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima (x:y:[]) = []
localMaxima (x:y:z:xs)
  | x > y = localMaxima (y:z:xs)
  | z > y = localMaxima (z:xs)
  | otherwise = y:(localMaxima (z:xs))

-- Exercise 3 Histogram

type HistItem = (Integer, Int)

group :: [Integer] -> [HistItem]
group xs = map (\l@(x:xs) -> (x, length l)) $ L.group $ L.sort xs

next :: [HistItem] -> [HistItem]
next [] = []
next ((c, 1):xs) = next xs
next ((c, count):xs) = (c, count-1):(next xs)

have :: Integer -> [HistItem] -> Bool
have _ [] = False
have n ((c, count):xs)
  | n == c = True
  | otherwise = have n xs

line :: [HistItem] -> String
line xs = map (\x -> if (have x xs) then '*' else ' ') [0..9]

draw :: [HistItem] -> [String]
draw [] = []
draw l = (line l):(draw $ next l)

histogram :: [Integer] -> String
histogram xs = (foldr (\x acc -> acc ++ x ++ "\n" )[] $ draw $ group xs) ++ "==========\n0123456789\n"

