{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage rawMessage
  | firstWord == "I"
      = LogMessage Info (read secondWord)
        (unwords (drop 2 rawMessageWords))
  | firstWord == "W"
      = LogMessage Info (read secondWord)
        (unwords (drop 2 rawMessageWords))
  | firstWord == "E"
      = LogMessage (Error (read secondWord)) (read thirdWord)
        (unwords (drop 3 rawMessageWords))
  | otherwise = Unknown (unwords rawMessageWords)
  where
    rawMessageWords = words rawMessage
    firstWord = head rawMessageWords
    secondWord = rawMessageWords !! 1
    thirdWord = rawMessageWords !! 2

parse :: String -> [LogMessage]
parse n = map parseMessage (lines n)
