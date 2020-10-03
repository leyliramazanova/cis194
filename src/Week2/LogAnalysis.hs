{-# OPTIONS_GHC -Wall #-}
module Week2.LogAnalysis where

import Week2.Log

{-
Exercise 1

Define a function
parseMessage :: String -> LogMessage
which parses an individual line from the log file. 

For example,
 parseMessage "E 2 562 help help"
     == LogMessage (Error 2) 562 "help help"
parseMessage "I 29 la la la"
     == LogMessage Info 29 "la la la"
 parseMessage "This is not in the right format"
     == Unknown "This is not in the right format"

Once we can parse one log message, we can parse a whole log file. 
Define a function parse :: String -> [LogMessage]
which parses an entire log file at once and returns its contents as a list of LogMessages.
-}

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

{-
Exercise 2 

Define a function
insert :: LogMessage -> MessageTree -> MessageTree
which inserts a new LogMessage into an existing MessageTree, pro- ducing a new MessageTree. 
insert may assume that it is given a sorted MessageTree, and must produce a new sorted MessageTree containing the new LogMessage in addition to the contents of the original MessageTree.
However, note that if insert is given a LogMessage which is Unknown, it should return the MessageTree unchanged.
-}

insert :: LogMessage -> MessageTree -> MessageTree
insert logmsg Leaf = Node Leaf logmsg Leaf
insert logmsg@(LogMessage _ tmstmp _) (Node left logmsg'@(LogMessage _ tmstmp' _) right)
    | tmstmp > tmstmp' = Node left logmsg' $ insert logmsg right
    | otherwise = Node (insert logmsg left) logmsg' right
insert _ tree = tree

{-
Exercise 3 

Once we can insert a single LogMessage into a MessageTree, we can build a complete MessageTree from a list of messages. Specifi- cally, define a function
build :: [LogMessage] -> MessageTree
which builds up a MessageTree containing the messages in the list, by successively inserting the messages into a MessageTree (beginning with a Leaf).
-}

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (l : ls') = insert l $ build ls'

{-
Exercise 4 

Finally, define the function inOrder :: MessageTree -> [LogMessage]
which takes a sorted MessageTree and produces a list of all the LogMessages it contains, sorted by timestamp from smallest to biggest. 
(This is known as an in-order traversal of the MessageTree.)
With these functions, we can now remove Unknown messages and sort the well-formed messages using an expression such as: inOrder (build tree)
-}

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

{-
Exercise 5 

Now that we can sort the log messages, the only thing left to do is extract the relevant information. 
We have decided that “relevant” means “errors with a severity of at least 50”.
Write a function
whatWentWrong :: [LogMessage] -> [String]
which takes an unsorted list of LogMessages, and returns a list of the messages corresponding to any errors with a severity of 50 or greater, sorted by timestamp. 
(Of course, you can use your functions from the previous exercises to do the sorting.)
-}

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ls = getMessage . inOrder . build . filter' $ ls