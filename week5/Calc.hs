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
