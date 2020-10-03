module Week3.Golf where

{- 
Exercise 1: Hopscotch

Your first task is to write a function skips :: [a] -> [[a]]
The output of skips is a list of lists. 
The first list in the output should be the same as the input list. 
The second list in the output should contain every second element from the input list. . . and the nth list in the output should contain every nth element from the input list.

For example:
skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips []           == []

Note that the output should be the same length as the input.

-}

{-
[1..length xs] indicates the number of iterations and also the indexes we run indexList on
-}
skips :: [a] -> [[a]]
skips xs = [indexList x xs | x <- [1..length xs]]

{-
This function takes an index and a list as its input.
Given an index x, we get the elements at that index - 1 and every xth element from the list.
Start with x-1 because when starting at 1, we want to make sure we get the 0th element on the first iteration.
Subsequently, on whatever index we started (x-1), we add x to it to extract the next element of the current sub-list.
-}
indexList :: Int -> [a] -> [a]
indexList x xs = [xs !! i | i <- [x-1, x-1+x..length xs - 1]]

exercise1 = do
  print $ skips "ABCD"       == ["ABCD", "BD", "C", "D"]
  print $ skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
  print $ skips [1]          == [[1]]
  print $ skips [True,False] == [[True,False], [False]]

{-
Exercise 2: Local maxima
A local maximum of a list is an element of the list which is strictly greater than both the elements immediately before and after it. 
For example, in the list [2,3,4,1,5], the only local maximum is 4, since it is greater than the elements immediately before and after it (3 and 1). 
5 is not a local maximum since there is no element that comes after it.

Write a function
localMaxima :: [Integer] -> [Integer]
which finds all the local maxima in the input list and returns them in order. For example:
 localMaxima [2,9,5,6,1] == [9,6]
 localMaxima [2,3,4,1,5] == [4]
 localMaxima [1,2,3,4,5] == 
-}

{-
Applying checkAtIndex only between index 1 and length-2 because the first and last element do not have elements on both sides to be compared to
-}

localMaxima :: [Integer] -> [Integer]
localMaxima xs = filter (/= 0) [checkAtIndex i xs | i <- [1..length xs -2]]

{-
Given an index and a list, this function will check if the element at the index is bigger than the neighbor elements on both sides.
-}

checkAtIndex :: Int -> [Integer] -> Integer
checkAtIndex i xs = if xs !! i  > xs !! (i-1) && xs !! i > xs !! (i+1) then xs !! i else 0

exercise2 = do
  print $ localMaxima [2,9,5,6,1] == [9,6]
  print $ localMaxima [2,3,4,1,5] == [4]
  print $ localMaxima [1,2,3,4,5] == []

{-
Exercise 3: Histogram
For this task, write a function
histogram :: [Integer] -> String
which takes as input a list of Integers between 0 and 9 (inclusive), and outputs a vertical histogram showing how many of each number were in the input list. 
You may assume that the input list does not contain any numbers less than zero or greater than 9 
(that is, it does not matter what your function does if the input does contain such numbers). 
Your output must exactly match the output shown in the examples below.

 histogram [1,1,1,5] ==
 *
 * **
 ==========
 0123456789

 histogram [1,4,5,4,6,6,3,4,2,4,9] ==
 *
 * **
  ******  *
 ==========
 0123456789


Important note: If you type something like histogram [3,5] at the ghci prompt, you should see something like this:
 "   * *    \n==========\n0123456789\n"
This is a textual representation of the String output, including \n escape sequences to indicate newline characters. 
To actually visualize the histogram as in the examples above, use putStr, for example, putStr (histogram [3,5]).
-}

histogram :: [Integer] -> String
histogram xs =   
    unlines (map (starLine num) [max, max-1..1]) ++ "==========\n0123456789\n"
    where 
        num = count xs
        max = maximum num



starLine :: [Int] -> Int -> String
starLine xs x = [if i >= x then '*' else ' ' | i <- xs]

{-inspired by https://codereview.stackexchange.com/questions/139587/count-occurrences-of-an-element-in-a-list-}
count :: [Integer] -> [Int]
count xs = map (\x -> length . filter (== x) $ xs) [0..9]

exercise3 = do
  print $ histogram [3,5] == "   * *    \n==========\n0123456789\n"

{-once overallTest is called, all output should say True-}
overallTest = do
    exercise1
    exercise2
    exercise3
