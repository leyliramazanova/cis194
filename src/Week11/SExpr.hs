module Week11.SExpr where

import Week11.AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- Runs consecutively the input parser as many times as possible and returns
-- a list of the results (always succeeds)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- Runs consecutively the input parser as many times as possible and returns
-- a list of the results (fails if it doesn't succeed at least once)

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

-- spaces should parse a consecutive list of zero or more whitespace characters

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

-- ident should parse an identifier, which for our purposes will be an alphabetic character 
-- (use isAlpha) followed by zero or more alphanumeric characters (use isAlphaNum)

ident :: Parser String
ident = (:) <$> satisfy isAlphaNum <*> zeroOrMore (satisfy isAlpha)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- S-expressions are a simple syntactic format for tree-structured data,
-- originally developed as a syntax for Lisp programs.
-- Write a parser for S-expressions

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> parseComb) <* spaces
  where
    parseAtom = A <$> (N <$> posInt <|> I <$> ident)
    parseComb = Comb <$> (char '(' *> many parseSExpr <* char ')')