module Dec_5 where

import Data.Char (isDigit, isLetter)
import Data.List.Extra
import Data.Maybe

day5 :: IO ()
day5 = do
    contents <- readFile "5th.txt"
    let bs = parseBlocks contents
        (sds, blocks) = (seeds $ concat $ head bs, reverse $ tail bs)
    print $ minimum $ map (\seed -> foldr findDest1 seed blocks) $ head $ head bs
    print $ fst $ minimum $ foldr findDest sds $ blocks

parseBlocks :: String -> [[[Int]]]
parseBlocks "" = []
parseBlocks str = nums : parseBlocks rest
    where (block_text, rest) = break (isLetter) $ dropWhile (not . isDigit) str
          nums = map toNumbers $ filter (any isDigit) $ lines block_text

toNumbers :: String -> [Int]
toNumbers str = map read $ words str


findDest1 :: [[Int]] -> Int -> Int
findDest1 ls src = if notNull found then head found else src
    where found = mapMaybe go ls
          go :: [Int] -> Maybe Int
          go line
            | src >= s && src <= (s+r) = Just (d + (src - s))
            | otherwise = Nothing
            where d = line !! 0
                  s = line !! 1
                  r = line !! 2


seeds :: [Int] -> [(Int,Int)]
seeds [] = []
seeds (_:[]) = []
seeds (n:m:ns) = (n,n+m) : seeds ns

findDest :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
findDest ls src = concat $ map go src
    where go :: (Int, Int) -> [(Int, Int)]
          go (sd1, sd2)
            | null lows && null ups = [(sd1, sd2)]
            | null lows = (sd1, sd2 - s up - 1) : [(d up, d up + (sd2 - s up))]
            | null ups = (d low + sd1 - s low, d low + r low - 1) : [(s low + r low + 1 - 1, sd2)]
            | low /= up = (d low + sd1 - s low, d low + r low - 1) : [(d up, d up + (sd2 - s up))]
            | otherwise = [(d low +(sd1-s low), d low + sd2 - s low)]
                where lows = filter (\ln -> sd1 >= (s ln) && sd1 <= (s ln + r ln)) ls
                      low = head $ lows
                      ups = filter (\ln -> sd2 >= (s ln) && sd2 <= (s ln + r ln)) ls
                      up = head $ ups
                      d l = l !! 0
                      s l = l !! 1
                      r l = l !! 2
