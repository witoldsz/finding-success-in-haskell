module Excersises25 where

import Data.Char
import Data.List

-- Excersise 8.7/25

main = do
  -- result <- checkAnagram <$> promptWord1 <*> promptWord2
  print result

promptWord1 :: IO String
promptWord1 = do
  putStr "Please enter a word.\n>"
  getLine


promptWord2 :: IO String
promptWord2 = do
  putStr "Please enter a second word.\n>"
  getLine

checkAnagram word1 word2 =
  case (isWord word1) of
    Nothing -> "The first word is invalid."
    Just word1 ->
      case (isWord word2) of
        Nothing -> "The second word is invalid."
        Just word2 ->
          case (isAnagram word1 word2) of
          False -> "These words are not anagrams."
          True -> "These words are anagrams."

isWord word =
  case (null word) of
  True -> Nothing
  False ->
    case (all isAlpha word) of
      False -> Nothing
      True -> Just word

isAnagram word1 word2 = (sort word1) == (sort word2)