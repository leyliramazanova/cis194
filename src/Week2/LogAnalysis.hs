{-# OPTIONS_GHC -Wall #-}
module Week2.LogAnalysis where

import Week2.Log

parseMessage :: String -> LogMessage
parseMessage s =
    let words_list = words s in
    case words_list of
    ("I":num:message) -> LogMessage Info (read num) (unwords message)
    ("W":num:message) -> LogMessage Warning (read num) (unwords message)
    ("E":val:num:message) -> LogMessage (Error (read val)) (read num) (unwords message)
    _ -> Unknown (unwords words_list)

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert logmsg Leaf = Node Leaf logmsg Leaf
insert logmsg@(LogMessage _ tmstmp _) (Node left logmsg'@(LogMessage _ tmstmp' _) right)
    | tmstmp > tmstmp' = Node left logmsg' $ insert logmsg right
    | otherwise = Node (insert logmsg left) logmsg' right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (l : ls') = insert l $ build ls'

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logmsg right) = (inOrder left) ++ [logmsg] ++ (inOrder right)

filter' :: [LogMessage] -> [LogMessage]
filter' []    = []
filter' (l@(LogMessage (Error val) _ _):ls')
    | val >= 50 = l : filter' ls'
filter' _ = []

getMessage :: [LogMessage] -> [String]
getMessage [] = []
getMessage ((LogMessage _ _ msg):ls') = msg : getMessage ls'
getMessage _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ls = getMessage . inOrder . build . filter' $ ls