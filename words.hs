-- words.hs - print wordlist and their counts from stdin
-- Copyright (C) 2022 Robert Coffey
-- Released under the GPLv3.

import Data.Function
import Data.List

wordList :: String -> [[String]]
wordList str = group . sort $ words str

wordCount :: [[String]] -> [(String, Int)]
wordCount lst = zip (map head lst) (map length lst)

wordString :: [(String, Int)] -> String
wordString lst = unlines $ map (\e -> (fst e) ++ "\t" ++ (show . snd $ e)) lst

main :: IO ()
main = interact $ wordString
                . sortBy (flip (compare `on` snd))
                . wordCount
                . wordList
