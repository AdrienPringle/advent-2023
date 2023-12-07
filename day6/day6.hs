main :: IO ()
main = do
  let input1 = "Time: 42 68 69 85\nDistance: 284 1005 1122 1341"
  let input2 = "Time: 42686985\nDistance: 284100511221341"
  
  print $ soln input1
  print $ soln input2
  
soln :: String -> Int
soln s = product $ map (uncurry winRange) (parseGames s)

parseGames :: String -> [(Int, Int)]
parseGames s = zip times distances
  where
    (times:(distances:[])) = map ( map read . tail . words) (lines s)

-- solve quadratic -x^2 + time * x - record
winRange :: Int -> Int -> Int
winRange time record = ihigh - ilow + 1
  where
    mid :: Double
    mid = fromIntegral time
    spread :: Double
    spread = sqrt (fromIntegral (time * time - (4 * record)))
    low = (mid - spread) * 0.5
    high = (mid + spread) * 0.5
    ilow
      | low == fromIntegral (ceiling low) = ceiling low + 1
      | otherwise = ceiling low
    ihigh
      | high == fromIntegral (floor high) = floor high - 1
      | otherwise = floor high
