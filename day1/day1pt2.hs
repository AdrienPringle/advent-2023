{-# LANGUAGE OverloadedStrings #-}


import Data.Char (isDigit, digitToInt)
import Data.Text(Text, pack, unpack, replace, isPrefixOf)

nums = [
  ("zero", 0),
  ("one", 1),
  ("two", 2),
  ("three", 3),
  ("four", 4),
  ("five", 5),
  ("six", 6),
  ("seven", 7),
  ("eight", 8),
  ("nine", 9)]

main :: IO ()
main = do
  l <- readFile "input.txt"
  putStrLn $ show $ sum . map (firstLast . listNums) $ lines l

listNums :: String -> [Int]
listNums [] = []
listNums (a:xs)
  | isDigit a = digitToInt a : listNums xs
  | otherwise = numericPrefix nums (pack (a : xs)) ++ listNums xs

numericPrefix :: [(Text, Int)] -> Text -> [Int]
numericPrefix [] c = []
numericPrefix ((a, b) : xs) c
  | isPrefixOf a c = [b]
  | otherwise = numericPrefix xs c

firstLast :: [Int] -> Int
firstLast n = head n * 10 + last n
