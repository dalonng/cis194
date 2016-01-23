-- Log file parsing


import Log

-- Exercise 1 The first step is figuring out how to parse an individual message.

parseMessage :: String -> LogMessage
parseMessage x = let flag = head $ words x
                     code = head $ drop 1 $ words x
                     code2 = head $ drop 2 $ words x
                     msg = tail $ drop 2 $ words x
                     msg2 = tail  $ drop 3 $ words x
                 in
                   case flag of
                     'E' : _ -> LogMessage (Error (read code)) (read code2) (unwords msg2)
                     'W' : _ -> LogMessage Warning (read code) (unwords msg)
                     'I' : _ -> LogMessage Info (read code) (unwords msg)
                     x -> Unknown x
                     

parse :: String -> [LogMessage]
parse ls = map parseMessage $ lines ls

-- Exercise 2 Define a Function

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l Leaf = Node Leaf l Leaf
insert msg@(LogMessage _ t _ ) tree@(Node left msg'@(LogMessage _ t' _) right)
  | t < t' = Node (insert msg left) msg' right
  | otherwise = Node left msg' (insert msg right)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build xs = foldr (insert) Leaf xs 

-- Exercise 4 Finally, define the Function

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right 

-- Exercise 5

errorHelper :: MessageType -> Int
errorHelper (Error e) = e
errorHelper _ = 0

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage mt _ s):xs)
  | mt == Info || mt == Warning = whatWentWrong xs
  | (errorHelper mt) > 50 = [s] ++ whatWentWrong xs
  | otherwise = whatWentWrong xs
