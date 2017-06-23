module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalParsedExp :: Maybe ExprT -> Maybe Integer
evalParsedExp Nothing = Nothing
evalParsedExp (Just i) = Just (eval i)

evalStr :: String -> Maybe Integer
evalStr = evalParsedExp . parseExp Lit Add Mul

-- Expr is a "type class," all of whose implementations
-- can do "lit", "add" and "mul"
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- Exprt is an instance of the Expr type class. Here's
-- how it does "lit", "add", and "mul
instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit a = Mod7 ((max a 0) `mod` 7)
  add (Mod7 a) (Mod7 b) = lit (lit a + lit b)
  mul (Mod7 a) (Mod7 b) = lit (lit a * lit b)
