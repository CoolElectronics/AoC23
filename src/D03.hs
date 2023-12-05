module D03 where

import Data.Bifunctor
import Data.Bifunctor (Bifunctor (second))
import Data.Char
import Data.List
import Debug.Trace
import System.IO
import Text.Read
import Util

digitlen :: String -> Int
digitlen [] = 0
digitlen line =
  if isDigit (head line)
    then 1 + digitlen (tail line)
    else 0

partsin :: Int -> (Int, String) -> [((Int, Int), Int, Int)]
partsin _ (_, []) = []
partsin x (y, line) =
  let first = head line
      dl = digitlen line
   in if isDigit first
        then [((y, x), read (take dl line), dl)] <> partsin (x + dl) (y, drop dl line)
        else [] <> partsin (x + 1) (y, tail line)

distance (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

pt1 text =
  let symbols =
        lines text
          |> enumerate
          |> map (second enumerate)
          |> map (second (filter (\x -> not (isDigit $ snd x) && (snd x /= '.')))) -- remove everything that isn't a symbol
          |> map (\x -> map (\y -> (fst x, fst y)) $ snd x) -- flatten to tuple with x,y
          |> concat
      isadjacent len x y = any (\i -> distance (fst x, snd x + i) y) [0 .. len - 1]
   in lines text
        |> enumerate
        |> map (partsin 0)
        |> concat
        |> filter (\(x, _, l) -> any (isadjacent l x) symbols)
        |> map (\(_, x, _) -> x)
        |> sum

pt2 text =
  let gears =
        lines text
          |> enumerate
          |> map (second enumerate)
          |> map (second (filter (\x -> snd x == '*'))) -- remove everything that isn't *
          |> map (\x -> map (\y -> (fst x, fst y)) $ snd x) -- flatten to tuple with x,y
          |> concat

      isadjacent len x y = any (\i -> distance (fst x, snd x + i) y) [0 .. len - 1]
      numbers = lines text |> enumerate |> map (partsin 0) |> concat
      gearadj y (x, _, l) = isadjacent l x y
      validgears = gears |> filter (\x -> sum (map ((\x -> if x then 1 else 0) . gearadj x) numbers) == 2)
   in validgears
        |> map (\g -> filter (gearadj g) numbers)
        |> map (\[(_, y, _), (_, x, _)] -> x * y)
        |> sum

d03 text = (pt1 text, pt2 text)
