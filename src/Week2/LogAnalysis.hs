{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
    let words_list = words s in
    case words_list of
    ("I":num:message) -> LogMessage Info (read num) (unwords message)
    ("W":num:message) -> LogMessage Warning (read num) (unwords message)
    ("E":val:num:message) -> LogMessage (Error (read val)) (read num) (unwords message)
    _ -> Unknown (unwords words_list)


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg@(Lo)




build :: [LogMessage] -> MessageTree


inOrder :: MessageTree -> [LogMessage]


whatWentWrong :: [LogMessage] -> [String]