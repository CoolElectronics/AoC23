import Data.Bifunctor
import Data.Char
import Data.List
import Debug.Trace
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
import Text.Read

x |> f = f x

splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

-- symbolchars = ['*', '$', '#', '+', '-', '-', '/', '\\', '%', '@']

enumerate = zip [0 ..]

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

main :: IO ()
main = do
  fileHandle <- openFile "inp.txt" ReadMode
  contents <- hGetContents fileHandle

  let symbols =
        lines contents
          |> enumerate
          |> map (second enumerate)
          |> map (second (filter (\x -> not (isDigit $ snd x) && (snd x /= '.')))) -- remove everything that isn't a symbol
          |> map (\x -> map (\y -> (fst x, fst y)) $ snd x) -- flatten to tuple with x,y
          |> concat

  let isadjacent len x y = any (\i -> distance (fst x, snd x + i) y) [0 .. len - 1]
  lines contents
    |> enumerate
    |> map (partsin 0)
    |> concat
    |> filter (\(x, _, l) -> any (isadjacent l x) symbols)
    |> map (\(_, x, _) -> x)
    |> sum
    |> print

  -- print symbols

  hClose fileHandle
