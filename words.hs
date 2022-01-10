{- words.hs - Get a list of words and their counts from a file. -}

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  mapM_ countFile args

countFile :: String -> IO ()
countFile path = do
  content <- readFile path
  printWordCount . sort . countWords . words $ content

printWordCount :: (Num n, Show n) => [(String, n)] -> IO ()
printWordCount [] = return ()
printWordCount (x:xs) = do
  putStr ((fst x) ++ ":\t")
  print (snd x)
  printWordCount xs

sort :: (Num n, Ord n) => [(String, n)] -> [(String, n)]
sort [] = []
sort l = sortByCount . sortByWord $ l

sortByWord :: Num n => [(String, n)] -> [(String, n)]
sortByWord [] = []
sortByWord (x:xs) =
  insertByWord x (sortByWord xs)

insertByWord :: Num n => (String, n) -> [(String, n)] -> [(String, n)]
insertByWord t [] = [t]
insertByWord t (x:xs) =
  if (fst t) > (fst x)
  then t : x : xs
  else x : insertByWord t xs

sortByCount :: (Num n, Ord n) => [(String, n)] -> [(String, n)]
sortByCount [] = []
sortByCount (x:xs) =
  insertByCount x (sortByCount xs)

insertByCount :: (Num n, Ord n) => (String, n) -> [(String, n)] -> [(String, n)]
insertByCount t [] = [t]
insertByCount t (x:xs) =
  if (snd t) > (snd x)
  then t : x : xs
  else x : insertByCount t xs

countWords :: Num n => [String] -> [(String, n)]
countWords l = countWords' l []

countWords' :: Num n => [String] -> [(String, n)] -> [(String, n)]
countWords' [] l = l
countWords' (x:xs) l = countWords' xs (keepFirst x (addWord x l))

keepFirst :: Num n => String -> [(String, n)] -> [(String, n)]
keepFirst w [] = []
keepFirst w (x:xs) =
  if w == fst x
  then x : dropWord w xs
  else x : keepFirst w xs

dropWord :: Num n => String -> [(String, n)] -> [(String, n)]
dropWord w [] = []
dropWord w (x:xs) =
  if w == fst x
  then xs
  else x : dropWord w xs

addWord :: Num n => String -> [(String, n)] -> [(String, n)]
addWord w l =
  case findPair w l of
    Just pair -> (fst pair, (snd pair)+1) : l
    Nothing -> (w, 1) : l

findPair :: Eq a => a -> [(a, b)] -> Maybe (a, b)
findPair t [] = Nothing
findPair t (x:xs) =
  if t == fst x
  then Just x
  else findPair t xs
