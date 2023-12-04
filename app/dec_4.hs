module Dec_4 where

import Data.Char (isDigit)

day4 :: IO ()
day4 = do
    contents <- readFile "4th.txt"
    let rows = lines contents

    putStr "Part 1: "
    print $ sum $ map points rows

    putStr "Part 2: "
    print $ sumCopies $ addCopies $ parseCards rows

-- 1
toNumbers :: String -> [Int]
toNumbers str = map read $ words str

points :: String -> Int
points game
    | num_wins game > 0 = 2^(num_wins game-1)
    | otherwise = 0

num_wins :: String -> Int
num_wins game = length $ filter (`elem` toNumbers wn ) $ toNumbers $ tail aw
    where (wn, aw) = break ( == '|') $ tail $ dropWhile ( ':' /= ) game

-- 2
data Card = C {game_id :: Int, wins :: Int, copies :: Int}
    deriving Eq

parseCards :: [String] -> [Card]
parseCards games = map ((\(x,y) -> C {game_id = x, wins = y, copies = 1} ) . card) games

card :: String -> (Int, Int)
card game = (game_id, num_wins game)
    where (g, _) = break (== ':') game
          game_id = read $ dropWhile (not . isDigit) g :: Int

addCopies :: [Card] -> [Card]
addCopies cards = foldl go cards cards
    where go :: [Card] -> Card -> [Card]
          go cs (C g w _) = cs'
            where affected = filter (\x -> (game_id x <= g+w) && (game_id x > g)) cs
                  c = copies $ head $ filter (\x-> game_id x == g) cs
                  updated = map (\ (C game wins c') -> (C game wins ((c'+c)))) affected
                  cs' = updated ++ filter (`notElem` affected) cs

sumCopies :: [Card] -> Int
sumCopies cards = foldl go 0 cards
    where go :: Int -> Card -> Int
          go s (C _ _ c) = s + c
