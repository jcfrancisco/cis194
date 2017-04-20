import Data.List
import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | n < 10    = [n]
  | otherwise = toDigits(div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse(toDigits n)

main :: IO ()
main = do
  print (toDigits 12340)
  print (toDigitsRev 256)
