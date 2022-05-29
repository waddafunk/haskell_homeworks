module Homework2 where

import Log

-- exercise 1

parseMessage :: String -> LogMessage
parseMessage s
  | h == "I" = LogMessage Info ts msg
  | h == "W" = LogMessage Warning ts msg
  | h == "E" = LogMessage (Error n) ts' msg'
  | otherwise = Unknown s
  where
    h = (head . words) s
    ts = read (words s !! 1) :: TimeStamp
    ts' = read (words s !! 2) :: TimeStamp
    n = ts :: Int
    msg = (unwords . drop 2 . words) s
    msg' = (unwords . drop 3 . words) s

parse :: String -> [LogMessage]
parse s = [parseMessage r | r <- lines s]

-- exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown s) tree = tree
insert message Leaf = Node Leaf message Leaf
insert msg@(LogMessage _ ts _) (Node left msg'@(LogMessage _ ts' _) right)
  | ts <= ts' = Node (insert msg left) msg' right
  | otherwise = Node left msg' (insert msg right)
insert _ tree = tree

-- exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- excercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder node@(Node left msg right) =
  case left of
    Leaf -> msg : inOrder right
    _ -> inOrder left ++ (msg : inOrder right)

-- exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong log = [s | (LogMessage (Error _) _ s) <- sortedLog]
  where
    sortedLog = (inOrder . build) log

-- testing
exampleTree :: MessageTree
exampleTree = insert m2 (insert m1 (insert m5 (insert m3 (insert m4 Leaf))))
  where
    m1 = LogMessage Info 1 " "
    m2 = LogMessage Info 2 " "
    m3 = LogMessage Info 3 " "
    m4 = LogMessage Info 4 " "
    m5 = LogMessage Info 5 " "