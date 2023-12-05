module D02 where

import Data.Char
import Data.List
import Debug.Trace
import System.IO
import Text.Read
import Util

procline :: [Char] -> (Int, Int, Int, Int)
procline line =
  let split = splitBy ':' line
      gamen = read $ last $ splitBy ' ' $ head split
      game = map (map (splitBy ' ' . tail) . splitBy ',') $ splitBy ';' $ last split
      ofcolor :: String -> [[String]] -> Int
      ofcolor col round = case find (\x -> last x == col) round of
        Just x -> read $ head x
        Nothing -> 0
      maxof :: ([[String]] -> Int) -> Int
      maxof predicate = maximum $ map predicate game
   in (gamen, maxof $ ofcolor "red", maxof $ ofcolor "green", maxof $ ofcolor "blue")

gamevalid (_, r, g, b) = r <= 12 && g <= 13 && b <= 14

powergame (_, r, g, b) = r * g * b

toid (i, _, _, _) = i

pt1 contents = sum $ map toid $ filter gamevalid $ map procline $ lines contents

pt2 contents = sum $ map (powergame . procline) $ lines contents

d02 contents = (pt1 contents, pt2 contents)
