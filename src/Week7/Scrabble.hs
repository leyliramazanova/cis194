{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Week7.Scrabble where

import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
  (<>) = (+) 

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score 
score c = 
    case c of
        'a' -> 1
        'b' -> 3
        'c' -> 3
        'd' -> 2
        'e' -> 1
        'f' -> 4
        'g' -> 2
        'h' -> 4
        'i' -> 1
        'j' -> 8
        'k' -> 5
        'l' -> 1
        'm' -> 3
        'n' -> 1
        'o' -> 1
        'p' -> 3
        'q' -> 10
        'r' -> 1
        's' -> 1
        't' -> 1
        'u' -> 1
        'v' -> 4
        'w' -> 4
        'x' -> 8
        'y' -> 4
        'z' -> 10
        _   -> 0

scoreString :: String -> Score
scoreString = foldl (\acc c -> acc + score c) (Score 0)
