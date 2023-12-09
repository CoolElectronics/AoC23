module D08 where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe (fromJust)
import Debug.Trace
import System.IO
import Text.Read
import Util

parse text =
  let [coord, rest] = splitOn " = " text
      [l, r] = map (filter isAlphaNum) $ splitOn ", " rest
   in (coord, (l, r))

findnexttgt s maps target = if s == 'L' then l else r
  where
    (coord, (l, r)) = fromJust $ find (\(c, _) -> c == target) maps

solverec predicate iterations maps (s : eq) target =
  if predicate nexttarget
    then iterations
    else solverec predicate (iterations + 1) maps (eq <> [s]) nexttarget
  where
    nexttarget = findnexttgt s maps target

pt1 seq maps = solverec (== "ZZZ") 1 maps seq "AAA"

rlcm :: [Int] -> Int
rlcm (x : s : xs) =
  if null xs
    then slv
    else rlcm ([slv] <> xs)
  where
    slv = lcm x s

pt2 seq maps =
  let startingnodes = filter (\x -> last x == 'A') $ map fst maps
   in rlcm $ map (solverec (\y -> last y == 'Z') 1 maps seq) startingnodes

d08 text =
  let lns = lines text
      seq = head lns
      maps = map parse $ drop 2 lns
   in (pt1 seq maps, pt2 seq maps)

-- naiive brute
-- solvemanyrec iterations targets maps (s : eq) =
--   if all (\x -> last x == 'Z') nexttgts
--     then iterations
--     else solvemanyrec (iterations + 1) nexttgts maps (eq <> [s])
--   where
--     nexttgts = traceShowId $ map (findnexttgt s maps) targets
