import Data.List
import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = toDigits(div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse(toDigits n)

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft []          = []
doubleEveryOtherFromLeft (x:[])      = [x]
doubleEveryOtherFromLeft (x:(y:zs))  = x : y * 2 : doubleEveryOtherFromLeft(zs)

-- Can also use native 'reverse' - did this just for fun
customReverse :: [Integer] -> [Integer]
customReverse []     = []
customReverse (x:xs) = customReverse(xs) ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = customReverse(doubleEveryOtherFromLeft(customReverse n))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[])
  | x < 10    = x
  | otherwise = sumDigits(toDigits(x))
sumDigits (x:(y:zs)) = sumDigits(toDigits(x)) + sumDigits(toDigits(y)) + sumDigits(zs)

validate :: Integer -> Bool
validate n  = mod
                (sumDigits(doubleEveryOther(toDigits n)))
                10
              == 0

type Peg = String
type Move = (Peg, Peg)
hanoiStep1 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiStep2 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiStep3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoiStep1 1 a b c = []
hanoiStep1 2 a b c = [(a, c)]
hanoiStep1 3 a b c = [(a, b), (a, c), (b, c)]

hanoiStep2 1 a b c = [(a, b)]
hanoiStep2 2 a b c = []
hanoiStep2 3 a b c = [(a, b)]

hanoiStep3 1 a b c = []
hanoiStep3 2 a b c = [(c, b)]
hanoiStep3 3 a b c = [(c, a), (c, b), (a, c)]

hanoi n a b c = (hanoiStep1 n a b c) ++ (hanoiStep2 n a b c) ++ (hanoiStep3 n a b c)

main :: IO ()
main = do
  print (toDigits 12340)
  print (toDigitsRev 256)
  print (doubleEveryOther [8,7,6,5])
  print (doubleEveryOther [1,2,3])
  print (sumDigits [16,7,12,5])
  print (validate 4012888888881881)
  print (validate 4012888888881882)
  print (hanoi 3 "a" "b" "c")
