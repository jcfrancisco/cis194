import Calc
import ExprT
import Parser

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "5 * 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


main :: IO ()
main = do
--  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
--  print $ evalStr "(2+3)*4"
  print $ testInteger
  print $ testBool
  print $ testMM
  print $ testSat
