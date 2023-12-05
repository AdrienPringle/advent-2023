import Data.List (intersect)

main :: IO ()
main = do
    l <- readFile "input.txt"
    print $ sum $ map (points . parseLine) (lines l)

points :: ([Int], [Int]) -> Int
points (l1, l2) = if null winning then 0 else 2 ^ (length winning - 1)
    where winning = l1 `intersect` l2

parseLine :: String -> ([Int], [Int])
parseLine l = (map read winners, map read havers)
    where
        nums = drop 2 (words l)
        (winners, _:havers) = break (=="|") nums
