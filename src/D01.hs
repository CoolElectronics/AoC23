module D01 where

import Data.Char
import Data.List
import Debug.Trace
import System.IO
import Text.Read

x |> f = f x

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

catends :: [Char] -> Int
catends x =
  read $ [head x] <> [last x]

digits = [("oneight", "18"), ("threeight", "38"), ("fiveight", "58"), ("sevenine", "79"), ("eightwo", "82"), ("twone", "21"), ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

digitsfind :: [(String, String)] -> String -> String -> Maybe (String, String)
digitsfind digits _head _tail =
  let firstchar = head _head
      digitvalid x =
        let needle = head $ fst x
         in firstchar == needle
      vaildigits = filter digitvalid digits
   in if null vaildigits
        then Nothing
        else
          let newdigits = map (\x -> (tail $ fst x, snd x)) vaildigits
              newhead = tail _head
              newtail = (_tail <> [firstchar])
           in case find (null . fst) newdigits of
                Just x ->
                  if isSingleton newdigits || null newhead -- but don't parse if there's nothing left
                    then Just (snd x, newhead) -- we only have one possible match, return it
                    else
                      let others = filter (not . null . fst) newdigits -- we have other matches, parse
                          othersol = digitsfind others newhead newtail
                       in case othersol of
                            Just x -> Just x
                            Nothing -> Just (snd x, newhead)
                Nothing ->
                  if null newhead
                    then Nothing
                    else digitsfind newdigits newhead newtail

digitsreplace :: String -> String
digitsreplace str =
  case digitsfind digits str "" of
    Nothing ->
      let first = head str
          newstr = tail str
       in if null newstr
            then
              if isDigit first
                then [first]
                else ""
            else
              let replaced = digitsreplace newstr
               in if isDigit first
                    then [first] <> replaced
                    else replaced
    Just x ->
      let num = fst x
          rest = snd x
       in if null rest
            then num
            else num <> digitsreplace rest

pt2 contents = sum $ map (catends . digitsreplace) $ lines contents

pt1 contents =
  sum
    ( map
        ( ( \x ->
              read ([head x] <> [last x])
          )
            . filter isDigit
        )
        (lines contents)
    )

d01 contents = (pt1 contents, pt2 contents)
