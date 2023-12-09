
import Data.Map (Map, fromList, (!))

main :: IO ()
main = do
    l <- readFile "input.txt"
    print $ uncurry traverseNodes (parseInput l)

traverseNodes :: [Bool] -> Map String (String, String) -> Int
traverseNodes steps nodes = aux "AAA" (cycle steps)
    where
        aux node stepCycle
            | node == "ZZZ" = 0
            | otherwise = 1 + aux nextNode remaining
                where
                    (nextDirection: remaining) = stepCycle
                    nextNode = getNextNode nodes node nextDirection

getNextNode :: Map String (String, String) -> String -> Bool -> String
getNextNode nodes currentNode direction = if direction then left else right
    where
        (left, right) = nodes ! currentNode

parseInput :: String -> ([Bool], Map String (String, String))
parseInput s = (parsedPath, fromList parsedNodes)
    where
        (path:(_:nodes)) = lines s
        parsedPath = map (=='L') path
        parsedNodes = map parseNode nodes


parseNode :: String -> (String, (String, String))
parseNode s = (name, (left, right))
    where
        (name, rest) = splitAt 3 s
        (left, rest2) = splitAt 3 (drop 4 rest)
        right = take 3 (drop 2 rest2)
