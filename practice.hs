-- excersie 10
reverseLine :: IO ()
-- reverseLine = getLine >>= (print . reverse)
reverseLine = do
  s <- getLine
  print $ reverse s

-- excersie 11
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma f =
  case ma of
    Nothing -> Nothing
    Just a -> f a

-- excersie 13
data StringOrValue a = Str String | Val a deriving Show

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue ma f =
  case ma of
    Str s -> Str s
    Val a -> f a