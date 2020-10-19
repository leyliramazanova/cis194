module Week6.Fibonacci where

{-
Exercise instructions for week6:
https://www.cis.upenn.edu/~cis194/spring13/lectures/06-laziness.html
-}

{-Exercise 1-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

{-Exercise 2-}

fibAux :: Integer -> Integer -> [Integer]
fibAux n1 n2 = n1 : fibAux n2 (n1 + n2)

fibs2 :: [Integer]
fibs2 = fibAux 0 1

{-Exercise 3-}

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons elem strm) = [elem] ++ streamToList strm

{-Exercise 4-}

streamRepeat :: a -> Stream a
streamRepeat elem = Cons elem $ streamRepeat elem

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons elem strm) = Cons (f elem) (streamMap f strm)

streamFromSeed :: (a -> a) -> a -> Stream a 
streamFromSeed f elem = Cons (elem) (streamFromSeed f (f elem))

{-Exercise 5-}

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons elem1 strm1) (Cons elem2 strm2) = Cons elem1 $ Cons elem2 $ interleaveStreams strm1 strm2

rulerInit :: Integer -> Stream Integer
rulerInit n = interleaveStreams (streamRepeat n) (rulerInit $ n + 1)

ruler :: Stream Integer
ruler = rulerInit 0


