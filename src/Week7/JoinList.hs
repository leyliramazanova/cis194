{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Week7.JoinList where

import Data.Monoid

import Week7.Buffer
import Week7.Editor
import Week7.Scrabble
import Week7.Sized

{-
Exercise 1 
We first consider how to write some simple operations on these JoinLists. 
Perhaps the most important operation we will consider is how to append two JoinLists. 
Previously, we said that the point of JoinLists is to represent append operations as data, but what about the annotations? 
Write an append function for JoinLists that yields a new JoinList whose monoidal annotation is derived from those of the two arguments.

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a

You may find it helpful to implement a helper function

tag :: Monoid m => JoinList m a -> m

which gets the annotation at the root of a JoinList.
-}

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m



(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

{-
Exercise 2 
The first annotation to try out is one for fast indexing into a JoinList. 
The idea is to cache the size (number of data ele- ments) of each subtree. 
This can then be used at each step to deter- mine if the desired index is in the left or the right branch.
We have provided the Sized module that defines the Size type, which is simply a newtype wrapper around an Int. 
In order to make Sizes more accessible, we have also defined the Sized type class which provides a method for obtaining a Size from a value.
Use the Sized type class to write the following functions. 1. Implement the function

indexJ :: (Sized b, Monoid b) =>
Int -> JoinList b a -> Maybe a

indexJ finds the JoinList element at the specified index. 
If the index is out of bounds, the function returns Nothing. By an index in a JoinList we mean the index in the list that it represents. 
That is, consider a safe list indexing function

(!!?) :: [a] -> Int -> Maybe a [] 
!!? _ = Nothing
!!? i | i < 0 = Nothing 
(x:xs) !!? 0 = Just x 
(x:xs) !!? i = xs !!? (i-1)

which returns Just the ith element in a list (starting at zero) if such an element exists, or Nothing otherwise. 
We also consider an updated function for converting join-lists into lists, just like jlbToList but ignoring the monoidal annotations:
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

We can now specify the desired behavior of indexJ. For any index i and join-list jl, it should be the case that
   (indexJ i jl) == (jlToList jl !!? i)
That is, calling indexJ on a join-list is the same as first convert- ing the join-list to a list and then indexing into the list. 
The point, of course, is that indexJ can be more efficient (O(log n) versus O(n), assuming a balanced join-list), because it gets to use the size annotations to throw away whole parts of the tree at once, whereas the list indexing operation has to walk over every element.

2. Implement the function

dropJ :: (Sized b, Monoid b) =>
Int -> JoinList b a -> JoinList b a

The dropJ function drops the first n elements from a JoinList. 
This is analogous to the standard drop function on lists. Formally, dropJ should behave in such a way that
jlToList (dropJ n jl) == drop n (jlToList jl).

3. Finally, implement the function takeJ :: (Sized b, Monoid b) =>
Int -> JoinList b a -> JoinList b a
The takeJ function returns the first n elements of a JoinList, dropping all other elements. 
Again, this function works similarly to the standard library take function; that is, it should be the case that
jlToList (takeJ n jl) == take n (jlToList jl).
Ensure that your function definitions use the size function from the Sized type class to make smart decisions about how to descend into the JoinList tree.

-}


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty  = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i _
   | i < 0 = Nothing
indexJ i (Append _ jl1 jl2)
   | i < jl1_size = indexJ i jl1
   | i > jl1_size + jl2_size = Nothing
   | otherwise    = indexJ (i - jl1_size) jl2
   where
      jl1_size = getSize . size . tag $ jl1
      jl2_size = getSize . size . tag $ jl2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ n (Append _ jl1 jl2)
   | n < jl1_size  = (dropJ n jl1) +++ jl2
   | n > jl1_size + jl2_size = Empty
   | n == jl1_size = jl2
   | otherwise     = dropJ (n - jl1_size) $ jl2
   where
      jl1_size = getSize . size . tag $ jl1
      jl2_size = getSize . size . tag $ jl2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ n (Append _ jl1 jl2)
   | n < jl1_size = takeJ n jl1
   | n > jl1_size + jl2_size = Empty
   | n == jl1_size = jl1
   | otherwise = jl1 +++ (takeJ (n - jl1_size) $ jl2)
   where
      jl1_size = getSize . size . tag $ jl1
      jl2_size = getSize . size . tag $ jl2

{-
Exercise 3:
Mr. Dickens’s publishing company has changed their
minds. Instead of paying him by the word, they have decided to pay him according to the scoring metric used by the immensely popular game of Scrabble.
ou must therefore update your editor implementation to count Scrabble scores rather than counting words.
Hence, the second annotation you decide to implement is one to cache the ScrabbleTM score for every line in a buffer. 
Create a Scrabble module that defines a Score type, a Monoid instance for Score, and the following functions:
score :: Char -> Score scoreString :: String -> Score
The score function should implement the tile scoring values as shown at http://www.thepixiepit.co.uk/scrabble/rules.html; any characters not mentioned (punctuation, spaces, etc.) should be given zero points.
To test that you have everything working, add the line import Scrabble to the import section of your JoinList module, and write the follow-
ing function to test out JoinLists annotated with scores:
scoreLine :: String -> JoinList Score String
Example:
 *JoinList> scoreLine "yay " +++ scoreLine "haskell!"
 Append (Score 23)
        (Single (Score 9) "yay ")
        (Single (Score 14) "haskell!")

-}

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

{-
Exercise 4 
Finally, combine these two kinds of annotations. A pair of monoids is itself a monoid:
 instance (Monoid a, Monoid b) => Monoid (a,b) where
   mempty = (mempty, mempty)
   mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)
(This instance is defined in Data.Monoid.) This means that join-lists can track more than one type of annotation at once, in parallel, simply by using a pair type.
Since we want to track both the size and score of a buffer, you should provide a Buffer instance for the type
JoinList (Score, Size) String.
Due to the use of the Sized type class, this type will continue to work with your functions such as indexJ.
Finally, make a main function to run the editor interface using your join-list backend in place of the slow String backend (see StringBufEditor.hs for an example of how to do this). 
You should create an initial buffer of type JoinList (Score, Size) String and pass it as an argument to runEditor editor. 
Verify that the editor demonstration described in the section “Editors and Buffers” does not exhibit delays when showing the prompt.
-}

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

instance Buffer (JoinList (Score, Size) String) where
   toString   = unlines . jlToList
   fromString xs = foldr1 (+++) $ createLine <$> lines xs
      where
         createLine s = Single (scoreString s, Size 1) s
   line = indexJ
   replaceLine _ "" b = b
   replaceLine n l b  = case indexJ n b of
      Nothing -> b
      Just _ -> takeJ n b +++ newline +++ dropJ (n + 1) b
      where
        newline = fromString l :: JoinList (Score, Size) String
   numLines = getSize . size . snd . tag
   value = (\(Score a) -> a) . fst . tag


