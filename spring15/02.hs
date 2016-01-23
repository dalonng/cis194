-- Polymorphism and Fuctional Programming Paradigms

-- Additional Syntax

-- let expressions

strLength :: String -> Int
strLength [] = 0
strLength (_:xs) = let len_rest = strLength xs
                   in len_rest + 1

-- where clauses

forb :: String -> Char
forb [] =  'a' -- len is NOT in scope here
forb str
  | len > 5 = 'x'
  | len < 3 = 'y'
  | otherwise = 'z'
  where len = strLength str 
