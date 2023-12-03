main :: IO ()

-- r g b
data Set = Set Int Int Int
  deriving Show

-- id sets
data Game = Game Int [Set]
  deriving Show

main = do
  l <- readFile "input.txt"

  let games = map parseGame (lines l)

  print $ sum (map (setPower . maxSetInList . gameSets) games)

gameSets :: Game -> [Set]
gameSets (Game _ s) = s

setPower :: Set -> Int
setPower (Set r g b) = r * g * b

maxSetInList :: [Set] -> Set
maxSetInList = foldr maxSet (Set 0 0 0)

maxSet :: Set -> Set -> Set
maxSet (Set r1 g1 b1) (Set r2 g2 b2) = Set (max r1 r2) (max g1 g2) (max b1 b2)

parseGame :: String -> Game
parseGame s = do
    let (idStr, gamesStr) = break (==':') s
    let id = (read . (!!1) . words) idStr :: Int
    let sets = (parseSetList . tail) gamesStr
    Game id sets

parseSetList :: String -> [Set]
parseSetList [] = []
parseSetList s = case break (==';') s of
    (a,[]) -> [parseSet (words a)]
    (a,b) -> parseSet (words a ): parseSetList (tail b)

parseSet :: [String] -> Set
parseSet [] = Set 0 0 0
parseSet (num:(color:next)) = joinSet (createSingleColorSet (read num) color) (parseSet next)

createSingleColorSet :: Int -> String -> Set
createSingleColorSet n ('r':_) = Set n 0 0
createSingleColorSet n ('g':_) = Set 0 n 0
createSingleColorSet n ('b':_) = Set 0 0 n

joinSet :: Set -> Set -> Set
joinSet s1 (Set 0 0 0) = s1
joinSet (Set 0 0 b) (Set r g _) = Set r g b
joinSet (Set 0 g 0) (Set r _ b) = Set r g b
joinSet (Set r 0 0 ) (Set _ g b) = Set r g b