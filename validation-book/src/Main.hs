module Main where

import           Data.Char
main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (validatePassword password)

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  case length password > 20 || length password < 10 of
    True -> Nothing
    False -> Just password

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  case all isAlphaNum xs of
    False -> Nothing
    True -> Just xs

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace (x : xs) =
  case isSpace x of
    True -> cleanWhitespace xs
    False -> Just (x : xs)

validatePassword :: String -> Maybe String
validatePassword password =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength
