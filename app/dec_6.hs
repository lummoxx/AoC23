module Dec_6 where

import Data.Char (isDigit, isLetter)

day6 :: IO ()
day6 = do
    contents <- readFile "6th.txt"
    print $ product $ map waysToWin $ parseRaces contents
    print $ waysToWin $ parseRace contents

data Race = R {time :: Int, distance :: Int}
    deriving Show

-- 1
parseRaces :: String -> [Race]
parseRaces str = map (\ (t, d) -> R t d) $ zip times distances
    where (first, rest) = break (isLetter) $ dropWhile (not . isDigit) str
          times = map read $ words first
          distances = map read $ words $ dropWhile (not . isDigit) rest

waysToWin :: Race -> Int
waysToWin (R t d) = length wins
    where wins = filter (\r -> distance r > d) $ map (\x -> R t ((t - x)*x)) [0..t]

-- 2
parseRace :: String -> Race
parseRace str = R (read $ head ts) (read $ last ts)
    where ts = map (filter isDigit) $ lines str