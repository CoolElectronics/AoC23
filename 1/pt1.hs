import Data.Char
import System.IO
import Text.Read

solve contents =
  sum
    ( map
        ( ( \x ->
              read ([head x] <> [last x])
          )
            . filter isDigit
        )
        (lines contents)
    )

main :: IO ()
main = do
  fileHandle <- openFile "inp.txt" ReadMode
  contents <- hGetContents fileHandle
  print (solve contents)

  hClose fileHandle
