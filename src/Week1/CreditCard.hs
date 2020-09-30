module Week1.CreditCard where

{-
 Exercise 1: We need to first find the digits of a number.

 1. toDigits should convert positive Integers to a list of digits. (For 0 or
 negative inputs, toDigits should return the empty list.)

 2. toDigitsRev should do the same, but with the digits reversed.

 Examples: toDigits 1234 == [1,2,3,4]
           toDigitsRev 1234 == [4,3,2,1]
           toDigits 0 == []
           toDigits (-17) == []
-}

toDigits :: Integer -> [Integer]
toDigits n 
    | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

{-
 Exercise 2: Once we have the digits in the proper order, we need to
 double every other one.

Remember that doubleEveryOther should double every other number beginning from the right,
that is, the second-to-last, fourth-to-last,. . . numbers are doubled.

 Examples: doubleEveryOther [8,7,6,5] == [16,7,12,5]
           doubleEveryOther [1,2,3] == [1,4,3]

 -}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [a] = [a]
doubleEveryOther (l : l' : ls')
    | length (l : l': ls') `mod` 2 == 0 = [2*l, l'] ++ doubleEveryOther ls'
    | length (l : l': ls') `mod` 2 /= 0 = [l, 2*l'] ++ doubleEveryOther ls'

{-
 Exercise 3 The output of doubleEveryOther has a mix of one-digit
 and two-digit numbers.

 Define the function:
   sumDigits :: [Integer] -> Integer
 to calculate the sum of all digits.

 Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
 -}

sumDigits :: [Integer] -> Integer
sumDigits ls = sum ls

{-
 Define the function:
   validate :: Integer -> Bool
 that indicates whether an Integer could be a valid credit card number.
 This will use all functions defined in the previous exercises.
 
 Example: validate 4012888888881881 = True
          validate 4012888888881882 = False
 -}

validate :: Integer -> Bool
validate n =
    let ls = toDigitsRev n in
    let ls_doubled = doubleEveryOther ls in
    let ls_doubled_clean = concatMap toDigits ls in
    let summed = sumDigits ls_doubled_clean in
    if summed `mod` 10 == 0 then True else False