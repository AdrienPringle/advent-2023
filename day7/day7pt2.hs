import Data.List (sort, group, sortBy, partition)
import Data.Ord (compare)

main :: IO ()
main = do
    l <- readFile "input.txt"
    print $ winnings $ sortCards $ map parseLine (lines l)

winnings :: [Int] -> Int
winnings l = sum (zipWith (*) [1..] l)

sortCards :: [(String, Int)] -> [Int]
sortCards = map snd . sortBy (\ i j -> compareCards (fst i) (fst j))

frequency :: Ord a => [a] -> [Int]
frequency = sortBy (flip compare) . map length . group . sort

parseLine :: String -> (String, Int)
parseLine s = (cards, read bid)
    where
        [cards, bid] = words s

compareCards :: String -> String -> Ordering
compareCards c1 c2
    | cmp1 == EQ = compareMaxOfCards c1 c2
    | otherwise = cmp1
    where
        h1 = handType c1
        h2 = handType c2
        cmp1 = compare (handType c1) (handType c2)

compareMaxOfCards :: String -> String -> Ordering
compareMaxOfCards [] [] = EQ
compareMaxOfCards s1 s2
    | cmp1 == EQ = compareMaxOfCards (tail s1) (tail s2)
    | otherwise = cmp1
    where
        cmp1 = compare (cardVal (head s1)) (cardVal (head s2))

cardVal :: Char -> Int
cardVal c
    | c == 'A' = 14
    | c == 'K' = 13
    | c == 'Q' = 12
    | c == 'J' = 1
    | c == 'T' = 10
    | otherwise = read [c]

handType :: String -> Int
handType s
    | maxFreq == 1 = 0 + 2*extra
    | take 2 cards == [2, 2] = 3 + 2*extra
    | maxFreq == 2 = 2 + 2*extra
    | cards == [3, 2] = 5
    | maxFreq == 3 = 4 + 2*extra
    | maxFreq == 4 = 6 + 2*extra
    | maxFreq == 5 = 8
    | extra == 5 = 8
        where
            (jacks, other) = partition (=='J') s
            cards = frequency other
            extra = length jacks
            maxFreq = if null cards then 0 else head cards
