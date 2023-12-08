module Dec_4 where

import Data.Char (isDigit)

day4 :: IO ()
day4 = do
    contents <- readFile "4th.txt"
    let rows = lines contents
    putStrLn $ "Part 1: " ++ (show $ sum $ map points rows)
    putStrLn $ "Part 2: " ++ (show $ sumCopies $ parseCards rows)

-- 1
toNumbers :: String -> [Int]
toNumbers str = map read $ words str

points :: String -> Int
points game
    | numWins game > 0 = 2^(numWins game-1)
    | otherwise = 0

numWins :: String -> Int
numWins game = length $ filter (`elem` toNumbers wn ) $ toNumbers $ tail aw
    where (wn, aw) = break ( == '|') $ tail $ dropWhile ( ':' /= ) game

-- 2
data Card = C {game_id :: Int, wins :: Int, copies :: Int}
    deriving Eq

parseCards :: [String] -> [Card]
parseCards games = map ((\(x,y) -> C x y 1 ) . card) games

card :: String -> (Int, Int)
card game = (game_id, numWins game)
    where game_id = read $ dropWhile (not . isDigit) $ takeWhile ( /= ':') game :: Int

sumCopies :: [Card] -> Int
sumCopies cards = foldl (\s (C _ _ c) -> s + c ) 0 $ foldl go cards cards
    where go :: [Card] -> Card -> [Card]
          go cs (C g w _) = cs'
            where affected = filter (\x -> (game_id x <= g+w) && (game_id x > g)) cs
                  c = copies $ head $ filter (\x-> game_id x == g) cs
                  updated = map (\ (C game wins c') -> (C game wins ((c'+c)))) affected
                  cs' = updated ++ filter (`notElem` affected) cs