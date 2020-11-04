module Week6.Fibonacci where

{-
Exercise instructions for week6:
https://www.cis.upenn.edu/~cis194/spring13/lectures/06-laziness.html
-}

{-
Exercise 1:
Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type
fib :: Integer -> Integer
so that fib n computes the nth Fibonacci number Fn.
Now use fib to define the infinite list of all Fibonacci numbers, fibs1 :: [Integer]
-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

{-
Exercise 2
Define the infinite list
 fibs2 :: [Integer]
so that it has the same elements as fibs1, but computing the first n elements of fibs2 requires only O(n) addition operations.
-}

fibs2 :: [Integer]
fibs2 = fibAux 0 1
    where 
        fibAux :: Integer -> Integer -> [Integer]
        fibAux n1 n2 = n1 : fibAux n2 (n1 + n2)

{-
Exercise 3
Define a data type of polymorphic streams, Stream.
• Write a function to convert a Stream to an infinite list,
streamToList :: Stream a -> [a]
• Make your own instance of Show for Stream, 
instance Show a => Show (Stream a) where show ...
which works by showing only some prefix of a stream (say, the first 20 elements).
-}

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons elem strm) = elem : streamToList strm

{-
Exercise 4
• Write a function
streamRepeat :: a -> Stream a
which generates a stream containing infinitely many copies of the given element.
• Write a function
streamMap :: (a -> b) -> Stream a -> Stream b which applies a function to every element of a Stream.
• Write a function
streamFromSeed :: (a -> a) -> a -> Stream a
which generates a Stream from a “seed” of type a, which is the first element of the stream, 
and an “unfolding rule” of type a -> a which specifies how to transform the seed into a new seed, 
to be used for generating the rest of the stream.
-}

streamRepeat :: a -> Stream a
streamRepeat elem = Cons elem $ streamRepeat elem

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons elem strm) = Cons (f elem) (streamMap f strm)

streamFromSeed :: (a -> a) -> a -> Stream a 
streamFromSeed f elem = Cons (elem) (streamFromSeed f (f elem))

{-
Exercise 5
• Define the stream
   nats :: Stream Integer
which contains the infinite list of natural numbers 0, 1, 2, . . .
• Define the stream
   ruler :: Stream Integer
which corresponds to the ruler function 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
where the nth element in the stream 
(assuming the first element corresponds to n = 1) is the largest power of 2 which evenly divides n.
-}

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons elem1 strm1) (Cons elem2 strm2) = Cons elem1 $ Cons elem2 $ interleaveStreams strm1 strm2

ruler :: Stream Integer
ruler = rulerInit 0
    where
        rulerInit :: Integer -> Stream Integer
        rulerInit n = interleaveStreams (streamRepeat n) (rulerInit $ n + 1)


