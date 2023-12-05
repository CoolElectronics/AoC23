module Main where

import D01 (d01)
import D02 (d02)
import D03 (d03)
import D04 (d04)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  fileHandle <- openFile ("data/" <> head args <> (if length args > 1 then "-ex" else "") <> ".txt") ReadMode
  contents <- hGetContents fileHandle
  let (pt1, pt2) = case args of
        "01" : _ -> d01 contents
        "02" : _ -> d02 contents
        "03" : _ -> d03 contents
        "04" : _ -> d04 contents
        _ -> error "uh"

  putStr "pt1: "
  print pt1
  putStr "pt2: "
  print pt2

  hClose fileHandle
