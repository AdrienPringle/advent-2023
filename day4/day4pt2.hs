import Data.List (intersect)

main :: IO ()
main = do
    l <- readFile "input.txt"
    print $ sum $ dupes $ map (winCount . parseLine) (lines l)

dupes :: [Int] -> [Int]
dupes wcount = aux (replicate l 0) (replicate l 1)
    where
        l = length wcount
        aux acc prev 
            | sum prev == 0 = acc
            | otherwise = aux newAcc next
            where
                next = buildNext wcount prev
                newAcc = zipWith (+) acc prev

buildNext :: [Int] -> [Int] -> [Int]
buildNext wcount cardCount = aux [] (reverse (zip cardCount wcount))
    where
        aux acc left
            | null left = acc
            | otherwise = aux newAcc newLeft
            where
                ((cards, wins): newLeft) = left
                newAcc = 0 : sumList (replicate wins cards) acc

sumList :: [Int] -> [Int] -> [Int]
sumList short long = zipWith (+) short eq ++ extra
    where
        (eq, extra) = splitAt (length short) long

winCount :: ([Int], [Int]) -> Int
winCount (l1, l2) = length (l1 `intersect` l2)

parseLine :: String -> ([Int], [Int])
parseLine l = (map read winners, map read havers)
    where
        nums = drop 2 (words l)
        (winners, _:havers) = break (=="|") nums
