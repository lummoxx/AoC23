module Dec_2 where
import Data.Char (isDigit, digitToInt)
import Data.List.Extra

day2 :: IO ()
day2 = do
    contents <- readFile "2nd.txt"
    print $ sum $ map game_id $ filter possible (lines contents)
    print $ sum $ map powerset (lines contents)

-- 1
possible :: String -> Bool
possible game = and [r < 13, g < 14, b < 15]
    where d = draws game
          r = red d
          g = green d
          b = blue d

draws :: String -> [[String]]
draws game = map (map trim . splitOn ",") $ splitOn ";" $ tail $ dropWhile ( ':' /= ) game

red :: [[String]] -> Int
red d =  maximum $ map (count "red") d

green :: [[String]] -> Int
green d =  maximum $ map (count "green") d

blue :: [[String]] -> Int
blue d = maximum $ map (count "blue") d

count :: String -> [String] -> Int
count colour draw
    | null digits = 0
    | otherwise = read digits
    where rightColour = filter (colour `isInfixOf`) draw
          digits = concat $ map (filter isDigit) rightColour

game_id :: String -> Int
game_id str = read $ takeWhile isDigit $ dropWhile (not . isDigit) str


-- 2
powerset :: String -> Int
powerset game = product $ [ f d | f <- [red, green, blue], d <- [draws game]]
