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

invertIntervals :: [[Int]] -> [[Int]]
invertIntervals intervals = map (\[a, b] -> [a + 1, b - a - 1]) $ zipWith (\[_, a] [b, _] -> [a, b]) intervals (tail intervals)

splits :: [[[Int]]] -> (Int, Int) -> Int
splits (mapping : rest) (start, len) =
  let ranges = (map (\[s, l] -> [s, s, l]) (invertIntervals ([[0, 1]] <> (map (\[dest, source, length] -> [source, length]) mapping) <> [[999, 1]]))) <> mapping
      ranges2 = filter (\[_, source, length] -> (start + len) >= source && start <= (source + length)) $ traceShowId ranges
      intersection dest source length = (max source start, min (source + length) (start + len) - max source start)
      rintersection dest source length = (fst (intersection dest source length) - source + dest, snd (intersection dest source length))
      interesctingranges = map (\[dest, source, length] -> rintersection dest source length) ranges2
   in traceShow
        (":" <> show ranges2 <> ":" <> show ranges <> ":" <> show interesctingranges)
        ( minimum $
            traceShowId $
              if null rest
                then map fst interesctingranges
                else map (\[dest, source, length] -> splits rest (rintersection dest source length)) ranges2
        )

-- (minimum $ if null rest then map (\[dest, source, length] -> fst (intersection dest source length)) ranges2 else (map (\[dest, source, length] -> splits rest (intersection dest source length)) ranges2))

pt2 text =
  let ([seeds] : maps) = parseinput text
      seedpairs = pairs seeds
   in map (splits maps) [head seedpairs]

d05 text = (pt1 text, pt2 text)
