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

  let gears =
        lines contents
          |> enumerate
          |> map (second enumerate)
          |> map (second (filter (\x -> snd x == '*'))) -- remove everything that isn't *
          |> map (\x -> map (\y -> (fst x, fst y)) $ snd x) -- flatten to tuple with x,y
          |> concat

  let isadjacent len x y = any (\i -> distance (fst x, snd x + i) y) [0 .. len - 1]
  let numbers = lines contents |> enumerate |> map (partsin 0) |> concat
  let gearadj y (x, _, l) = isadjacent l x y
  let validgears = gears |> filter (\x -> sum (map ((\x -> if x then 1 else 0) . gearadj x) numbers) == 2)

  validgears
    |> map (\g -> filter (gearadj g) numbers)
    |> map (\[(_, y, _), (_, x, _)] -> x * y)
    |> sum
    |> print

  hClose fileHandle
