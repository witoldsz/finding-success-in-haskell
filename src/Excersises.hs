module Excersises where

promptWord1 :: IO String
promptWord1 = do
  putStr "Please enter a word.\n> "
  getLine

promptWord2 :: IO String
promptWord2 = do
  putStr "Please enter a second word.\n> "
  getLine

main:: IO ()
main = do
  result <- (++) <$> promptWord1 <*> promptWord2
  print result