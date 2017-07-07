import Data.Foldable

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fib2 :: (Integer, Integer) -> (Integer, Integer)
fib2 (x, y) = (x + y, x)

fibs2 :: [Integer]
fibs2 = map fst (iterate fib2 (0, 1))

main :: IO ()
main = do
  print $ take 100 fibs2

