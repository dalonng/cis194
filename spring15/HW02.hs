 -- Homework 02

data Peg = Red
          | Green
          | Blue
          | Yellow
          | Orange
          | Purple
          deriving (Show, Eq, Ord)

type Code = [Peg]

 -- Exercise 1

exactMatches :: Code -> Code -> Int
exactMatches a b = length . filter (uncurry (==)) $ zip a b 


-- Exercise 2

coloers :: [Peg]
coloers = [Red, Green, Blue, Yellow, Orange, Purple]

countColors :: Code -> [Int]
countColors ps = map (\x -> length $ filter (==x) ps ) coloers

matches :: Code -> Code -> Int
matches
