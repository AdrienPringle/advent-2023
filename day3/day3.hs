import Data.Char (isNumber)
import Text.Parsec (parse, satisfy)
import Distribution.Compat.Graph (neighbors)

-- x y val
data PartNum = PartNum Int Int String
  deriving Show

main :: IO ()
main = do

    l <- readFile "input.txt"
    let lin = lines l

    print $ (sum . map partVal . filter (isValidPart lin) . parseNums ) lin

partVal :: PartNum -> Int
partVal (PartNum _ _ s) = read s

isValidPart :: [String] -> PartNum -> Bool
isValidPart s p = any (isSymbol s) validCoords
    where
        maxY = length s
        maxX = length (head s)
        validCoords = filter (isCoordInRange maxX maxY) (neighbourCoords p)

isSymbol :: [String] -> (Int, Int) -> Bool
isSymbol s (x, y) = ((s !! y) !! x) /= '.'

isCoordInRange :: Int -> Int -> (Int, Int) -> Bool
isCoordInRange maxX maxY (x, y) = (x < maxX) && (y < maxY) && (x >= 0) && (y >= 0)

neighbourCoords :: PartNum -> [(Int, Int)]
neighbourCoords p = left : right : up ++ down
    where
        (PartNum x y s) = p
        left = (x-1, y)
        right = (x + length s,y)
        up = map (,y+1) [x-1 .. x + length s]
        down = map (,y-1) [x-1 .. x + length s]


parseNums :: [String] -> [PartNum]
parseNums s = concatMap (uncurry lineNums) (zip [0..] s)

lineNums :: Int -> String -> [PartNum]
lineNums y inits = aux inits 0 y
    where
        aux s x y
            | null s = []
            | null prefix = aux (tail rest) (x + 1) y
            | otherwise = PartNum x y prefix : aux rest (x + length prefix) y
            where
                (prefix, rest) = span isNumber s





