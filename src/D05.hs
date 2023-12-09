module D05 where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace
import System.IO
import Text.Read
import Util

parseinput :: [Char] -> [[[Int]]]
parseinput text = map (\x -> map (map read) $ filter (\x -> (x /= []) && (head x /= "")) $ map (splitBy ' ') $ splitBy '\n' (last $ splitBy ':' x)) $ splitOn "\n\n" text

inmap val [_, source, length] = val >= source && val <= source + length

findmapped :: [[Int]] -> Int -> Int
findmapped mapping val =
  let [dest, source, _] = fromMaybe [0, 0, 0] $ find (inmap val) mapping
   in (val - source) + dest

findmappedrec [] s = s
findmappedrec (map : maps) seed = findmappedrec maps (findmapped map seed)

pt1 text =
  let ([seeds] : maps) = parseinput text
   in minimum $ map (findmappedrec maps) seeds

splits :: [[[Int]]] -> (Int, Int) -> Int
splits _ (9999999, _) = 9999999
splits (mapping : rest) (start, len) =
  let ranges = filter (\[_, source, length] -> (start + len) >= source && start <= (source + length)) mapping

      ranges3 = map (\[_, source, length] -> [start, start, source]) $ filter (\[_, source, length] -> start < source) ranges
      ranges6 = map (\[_, source, length] -> [start + len, start + len, (start + len) - (source + length)]) $ filter (\[_, source, length] -> (start + len) > (source + length)) ranges
      ranges4 = ranges3 <> ranges <> ranges6
      ranges2 = if null ranges4 then [[start, start, len]] else ranges4
      intersection dest source length = (max source start, min (source + length) (start + len) - max source start)
      rintersection dest source length = (fst (intersection dest source length) - source + dest, snd (intersection dest source length))
      interesctingranges = map (\[dest, source, length] -> rintersection dest source length) ranges2
   in -- traceShow
      -- (":" <> show ranges2 <> ":" <> show ranges <> ":" <> show interesctingranges)
      ( minimum $
          if null rest
            then map fst interesctingranges
            else map (\[dest, source, length] -> splits rest (rintersection dest source length)) ranges2
      )

-- (minimum $ if null rest then map (\[dest, source, length] -> fst (intersection dest source length)) ranges2 else (map (\[dest, source, length] -> splits rest (intersection dest source length)) ranges2))

pt2 text =
  let ([seeds] : maps) = parseinput text
      seedpairs = pairs seeds
   in minimum $ map (splits maps) seedpairs

d05 text = (pt1 text, pt2 text)
