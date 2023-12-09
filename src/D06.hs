module D06 where

import Data.Char
import Data.List
import Debug.Trace
import System.IO
import Text.Read
import Util

parseRace text = splitBy " " $ last $ splitBy ":" text

pt1 text = 1

pt2 text = 1

d06 text = (pt1 text, pt2 text)
