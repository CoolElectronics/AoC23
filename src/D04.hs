module D04 where

import Data.Char
import Data.List
import Debug.Trace
import System.IO
import Text.Read
import Util

parsecard line =
  let [gn, rest] = take 2 $ splitBy ':' line
      [winning, have] = map (filter (/= "") . splitBy ' ') $ take 2 $ splitBy '|' rest
   in length $ filter (`elem` winning) have

pointsof 0 = 0
pointsof num = 2 ^ (num - 1)

pt1 text = sum $ map (pointsof . parsecard) (lines text)

nextcard [] = 0
nextcard [_] = 1
nextcard (x : xs) = 1 + sum (map nextcard $ take x $ tails xs)

pt2 text = sum $ map nextcard $ tails (map parsecard $ lines text)

d04 text = (pt1 text, pt2 text)
