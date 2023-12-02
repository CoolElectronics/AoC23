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

splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

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

powergame (_, r, g, b) = r * g * b

main :: IO ()
main = do
  fileHandle <- openFile "inp.txt" ReadMode
  contents <- hGetContents fileHandle

  print $ sum $ map (powergame . procline) $ lines contents

  hClose fileHandle
