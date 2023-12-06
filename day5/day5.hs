import Data.Maybe (fromMaybe)
import Control.Monad (msum)

main :: IO ()
main = do
    l <- readFile "input.txt"
    print $ (minimum . mapsoln . parseInput) l

mapsoln :: ([Int], [[(Int, Int, Int)]]) -> [Int]
mapsoln (nums, rules) = map (`applyAllRules` rules) nums

applyAllRules :: Int -> [[(Int, Int, Int)]] -> Int
applyAllRules = foldl applyRule

applyRule :: Int -> [(Int, Int, Int)] -> Int
applyRule seed rules = fromMaybe seed (msum (map (testRule seed) rules))

testRule :: Int -> (Int, Int, Int) -> Maybe Int
testRule seed (dest, src, len)
    | (seed >= src) && (seed < src + len) = Just (seed + dest - src)
    | otherwise = Nothing

parseInput :: String -> ([Int], [[(Int, Int, Int)]])
parseInput s = (seedInts, reverse ruleList)
    where
        (seeds:rules) = lines s
        seedInts = (map read . tail . words) seeds
        ruleList = aux [] [] (drop 2 rules)
            where
                aux acc next [] = next : acc
                aux acc next [""] = next : acc
                aux acc next ("":(_:superNext)) = aux (next : acc) [] superNext
                aux acc next (line:superNext) = aux acc (parseLine line:next) superNext
                parseLine l = (dest, src, len)
                    where
                        [dest, src, len] = (map read . words) l
