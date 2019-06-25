module Excersises24 where

import           Data.Char

-- Excersise 8.7/24

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  case length password > 20 of
    True -> Left "Your password cannot be longer than 20 characters."
    False -> Right password

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Cannot be empty."
cleanWhitespace (x : xs) =
  case isSpace x of
    True -> cleanWhitespace xs
    False -> Right (x : xs)

requireAlphaNum :: String -> Either String String
requireAlphaNum xs =
  case all isAlphaNum xs of
    False -> Left "Cannot contain white space or special characters."
    True -> Right xs

validatePassword_monadic :: String -> Either String String
validatePassword_monadic password =
    cleanWhitespace password
      >>= requireAlphaNum
      >>= checkPasswordLength

validatePassword :: String -> Either String String
validatePassword password =
    case (cleanWhitespace password) of -- Either String String
        Left err -> Left err
        Right p2 -> requireAlphaNum p2 *> checkPasswordLength p2
