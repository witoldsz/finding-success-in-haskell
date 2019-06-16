module Main where

import           Data.Char
main :: IO ()
main = do
  putStr "Please enter a password\n> "
  password <- getLine
  print (validatePassword password)

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case length password > 20 of
    True -> Left "Your password cannot be longer than 20 characters."
    False -> Right password

requireAlphaNum :: String -> Either String String
requireAlphaNum xs =
  case all isAlphaNum xs of
    False -> Left "Your password cannot contain\
                  \white space or special characters."
    True -> Right xs

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Your password cannot be empty"
cleanWhitespace (x : xs) =
  case isSpace x of
    True -> cleanWhitespace xs
    False -> Right (x : xs)

validatePassword :: String -> Either String String
validatePassword password =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength
