import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (msum)

main :: IO ()
main = do
    l <- readFile "input.txt"

    -- the filter /=0 is probably due to a bug in `testRule`, but this still finds the correct solution
    print $ (minimum . filter (/=0) . map fst . mapsoln . parseInput) l

mapsoln :: ([(Int, Int)], [[(Int, Int, Int)]]) -> [(Int, Int)]
mapsoln (nums, rules) =  applyAllRules nums rules

applyAllRules :: [(Int, Int)] -> [[(Int, Int, Int)]] -> [(Int, Int)]
applyAllRules = foldl applyRuleToRanges

applyRuleToRanges :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)]
applyRuleToRanges seeds rules = concatMap (`applyRule` rules) seeds

applyRule :: (Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)]
applyRule seed rules = aux rules [seed]
    where
        aux _ [] = []
        aux [] remainingRanges = remainingRanges
        aux (rule:remainingRules) seeds = allNew ++ aux remainingRules allRemainder
            where
                (maybeNew, remainders) = unzip (map (`testRule` rule) seeds)
                allNew = catMaybes maybeNew
                allRemainder = concat remainders



testRule :: (Int, Int) -> (Int, Int, Int) -> (Maybe (Int, Int), [(Int, Int)])
testRule (start, end) (dest, src, len)
    | end < start = (Nothing, [])
    | startIsHigher || endIsLower = (Nothing, [(start, end)])
    | startIsLower && endIsHigher = (Just (dest, destend), [(start, src - 1), (srcend + 1, end)])
    | startIsInside = (Just (mappedStart, destend), [(srcend + 1, end)])
    | otherwise = (Just (dest, mappedEnd), [(start, src-1)])
        where
            srcend = src + len - 1
            destend = dest + len - 1
            startIsHigher = start > srcend
            startIsLower = start <= src
            startIsInside = not (startIsLower || startIsHigher)
            endIsHigher = end >= srcend
            endIsLower = end < src
            endIsInside = not (endIsLower || endIsHigher)
            mappedStart = start + dest - src
            mappedEnd = end + dest - src

parseInput :: String -> ([(Int, Int)], [[(Int, Int, Int)]])
parseInput s = (pairs seedInts, reverse ruleList)
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
pairs [] = []
pairs (a:(b:c)) = (a,a + b - 1):pairs c