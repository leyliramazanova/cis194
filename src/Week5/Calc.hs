{-# LANGUAGE FlexibleInstances #-}
module Week5.Calc where
import Week5.ExprT
import Week5.Parser
import Week5.StackVM

{-
NOTE: The exercise instructions are quite lengthy.
So, here's the link to week 5 instructions for reference:
https://www.cis.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf
-}

{-Exercise 1-}
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Week5.ExprT.Add e1 e2) = eval e1 + eval e2
eval (Week5.ExprT.Mul e1 e2) = eval e1 * eval e2

{-Exercise 2-}

evalStr :: String -> Maybe Integer
evalStr e = case parseExp Lit Week5.ExprT.Add Week5.ExprT.Mul e of
    Just a -> Just $ eval a
    Nothing -> Nothing

{-Exercise 3-}
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Week5.ExprT.Add
    mul = Week5.ExprT.Mul

reify :: ExprT -> ExprT
reify = id

{-Exercise 4-}

instance Expr Integer where
    lit i = i
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

data MinMax = MinMax Integer 
    deriving(Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax e1) (MinMax e2) = MinMax (max e1 e2)
    mul (MinMax e1) (MinMax e2) = MinMax (min e1 e2)

data Mod7 = Mod7 Integer
    deriving(Eq, Show)

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 e1) (Mod7 e2) = Mod7 ((e1 + e2) `mod` 7)
    mul (Mod7 e1) (Mod7 e2) = Mod7 ((e1 * e2) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = print (testExp :: Maybe Integer)
testBool = print (testExp :: Maybe Bool)
testMM =  print (testExp :: Maybe MinMax)
testSat =  print (testExp :: Maybe Mod7)

{-Exercise 5-}

instance Expr Week5.StackVM.Program where
    lit n = [Week5.StackVM.PushI n]
    add e1 e2 = e1 ++ e2 ++ [Week5.StackVM.Add]
    mul e1 e2 = e1 ++ e2 ++ [Week5.StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

testProgram = testExp :: Maybe Program






