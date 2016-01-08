-- Golf.hs

module Golf where

-- Exercise 1 Hopscotch

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n xs
  | n <= length xs = (last $ take n xs): (everyNth n (drop n xs))
  | otherwise = []

uuu :: Int -> [a] ->[[a]]
uuu 0 _ = []
uuu _ [] = []
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
