import Data.Char (isDigit)
import Data.Text(Text, pack, unpack, replace, isPrefixOf)

main :: IO ()
main = do
  l <- readFile "input.txt"
  putStrLn $ show $ sum . map firstLast $ lines l

firstLast :: String -> Int
firstLast n = do
  let filtered = filter isDigit n
  read [head filtered, last filtered]
  