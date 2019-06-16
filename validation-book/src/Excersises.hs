module Excersises where

import Main (checkPasswordLength)

printTestResult :: Either String () -> IO ()
printTestResult r =
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  case (actual == expected) of
    True -> Right ()
    False -> Left (unlines
      [ "Test " ++ show n
      , "  Expected:  " ++ show expected
      , "  But got:  " ++ show actual
      ])

test :: IO ()
test = printTestResult $
  do
    eq 1 (checkPasswordLength "") (Right "")
    eq 2 (checkPasswordLength "julielovesbooks")
         (Right "julielovesbooks")

