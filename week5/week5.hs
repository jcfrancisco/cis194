import Calc
import ExprT

main :: IO ()
main = do
  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
  print $ evalStr "(2+3)*4"
