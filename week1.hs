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

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse(doubleEveryOtherFromLeft(reverse n))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:(y:zs)) = sumDigits(toDigits(x)) + sumDigits(toDigits(y)) + sumDigits(zs)

validate :: Integer -> Bool
validate n  = mod
                (sumDigits(doubleEveryOther(toDigits n)))
                10
              == 0

main :: IO ()
main = do
  print (toDigits 12340)
  print (toDigitsRev 256)
  print (doubleEveryOther [8,7,6,5])
  print (doubleEveryOther [1,2,3])
  print (sumDigits [16,7,12,5])
  print (validate 4012888888881881)
  print (validate 4012888888881882)
