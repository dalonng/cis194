-- Recursion patterns, polymorphism, and the Prelude

-- 回想一个之前我们定义的简单的整数列表:

data IntList = Empty | Cons Int IntList
             deriving Show


-- Map
-- Let's think about the first one ("perform some operation on every element of the list"). For example, we could add one to every element in a list:
-- Or we could ensure that every element in a list is nongegative by taking the absolute value:

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) $ absAll xs

-- Or we could square every element:

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1
square x = x * x

-- Polymorphism

-- Polymorphic data types

data List t = E | C t (List t)

-- lst1 :: (List Int)
lis1 = C 3 (C 5 (C 3 E))

-- lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

-- lst3 :: List Bool
lst3 = C True (C False E)

filterList _ E = E
filterList p (C x xs)
  | p x = C x (filterList p xs)
  | otherwise = filterList p xs
