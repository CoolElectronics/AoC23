module Util where

x |> f = f x

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

pairs [] = []
pairs (x:y:xs) = [(x,y)] <> pairs xs
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
