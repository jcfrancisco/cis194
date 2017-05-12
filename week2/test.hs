import LogAnalysis

main :: IO ()
main = do
  print (parseMessage "E 2 562 help help")
  print (parseMessage "I 29 la la la")
  print (parseMessage "This is not in the right format")
