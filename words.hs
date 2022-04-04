-- words.hs - print wordlist and their counts for each file
-- Copyright (C) 2022 Robert Coffey
-- Released under the GPLv3.

import Control.Monad
import Data.Function
import Data.List
import System.Environment

wordList :: String -> [[String]]
wordList = group . sort . words

wordCount :: [[String]] -> [(String, Int)]
wordCount lst = [(head x, length x) | x <- lst]

wordString :: [(String, Int)] -> String
wordString = unlines . map (\e -> (fst e) ++ "\t" ++ (show . snd $ e))

countWords :: String -> String
countWords = wordString
           . sortBy (flip (compare `on` snd))
           . wordCount
           . wordList

main :: IO ()
main = do
  args <- getArgs
  if length args > 0
    then do
      str <- liftM (countWords . concat) $ mapM readFile args
      putStr str
    else interact countWords
  return ()
