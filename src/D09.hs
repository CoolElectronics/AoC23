module D09 where

import Data.Char
import Data.List
import Data.Maybe (isNothing)
import Debug.Trace
import System.IO
import Text.Read
import Util

parse text = map (map read . splitBy ' ') $ lines text

delta [_] = []
delta (f : s : rest) = [(f - s)] <> delta ([s] <> rest)

allTheSame [] = True
allTheSame (x : xs) = isNothing $ find (x /=) xs

rdelta :: [Int] -> [[Int]]
rdelta d =
  ( if allTheSame dt
      then [dt]
      else rdelta dt
  )
    <> [d]
  where
    dt = delta d

predict :: [[Int]] -> [[Int]]
predict [x : _, y : _] = [[x + y]]
predict ((x : xs) : (y : ys) : s) = predict ([[x + y]] <> s)

solve d =
  head $ head $ predict $ map reverse tree
  where
    tree = map reverse $ rdelta d

pt1 lines = sum $ map (solve . reverse) lines

-- lmfao
pt2 lines = sum $ map solve lines

d09 text = (pt1 parsed, pt2 parsed) where parsed = parse text
