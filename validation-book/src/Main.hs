module Main where

import           Data.Char

newtype Password = Password String
  deriving Show

newtype Error = Error String
  deriving Show

newtype Username = Username String
  deriving Show

data User = User Username Password
  deriving Show

main :: IO ()
main = do
  putStr "Please enter a username\n> "
  username <- Username <$> getLine
  putStr "Please enter a password\n> "
  password <- Password <$> getLine
  print (makeUser username password)

makeUser :: Username -> Password -> Either Error User
makeUser name password =
  User <$> validateUsername name
       <*> validatePassword password

checkPasswordLength :: String -> Either Error Password
checkPasswordLength password =
  case length password > 20 of
    True -> Left (Error "Your password cannot be longer than 20 characters.")
    False -> Right (Password password)

checkUsernameLength :: String -> Either Error Username
checkUsernameLength name =
  case (length name > 15) of
    True -> Left (Error "Username cannot be longer than 15 characters.")
    False -> Right (Username name)

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
  case all isAlphaNum xs of
    False -> Left (Error "Cannot contain white space or special characters.")
    True -> Right xs

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left (Error "Cannot be empty.")
cleanWhitespace (x : xs) =
  case isSpace x of
    True -> cleanWhitespace xs
    False -> Right (x : xs)

validatePassword :: Password -> Either Error Password
validatePassword (Password password) =
  cleanWhitespace password
    >>= requireAlphaNum
    >>= checkPasswordLength

validateUsername :: Username -> Either Error Username
validateUsername (Username username) =
  cleanWhitespace username
    >>= requireAlphaNum
    >>= checkUsernameLength
