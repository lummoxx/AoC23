module Dec_3 where

import Data.Char (isDigit)

day3 :: IO ()
day3 = do
    contents <- readFile "3rd.txt"
    let rows = zip [0..] $ map (zip [0..]) $ lines contents

    putStr "Part 1: "
    print $ sum $ realParts rows

    putStr "Part 2: "
    print $ sum $ gearRatios rows

-- 1
data Part = P {v :: Int, is :: [Int], r :: Int }

realParts :: [(Int, [(Int, Char)])] -> [Int]
realParts rows = map v $ filter real parts
    where sym_indice = map (\x -> (fst x, symbols $ snd x)) $ rows
          parts = allParts rows
          real :: Part -> Bool
          real (P _ ix row) = check ix rel_sym
            where rel_sym = concat $ map snd $ filter (\x -> fst x `elem` (adj row)) sym_indice

check :: [Int] -> [Int] -> Bool
check ni si = or $ map (`elem` si) ni'
    where ni' = (minimum ni - 1) : (maximum ni + 1) : ni

symbols :: [(Int, Char)] -> [Int]
symbols = map fst . filter (\x -> (snd x) /= '.') . filter (not . isDigit . snd)

allParts :: [(Int, [(Int, Char)])] -> [Part]
allParts [] = []
allParts ((row, cols):rs) = map (\x -> P {v = read $ snd x, is = fst x, r = row}) parts ++ allParts rs
    where parts = map unzip $ partition cols

partition ::[(Int,Char)] -> [[(Int,Char)]]
partition [] = []
partition ls
    | null first = partition rest
    | any (isDigit . snd) dropped = first : partition rest
    | otherwise = []
        where dropped = dropWhile (not . isDigit . snd) ls
              (first, rest) = break (not . isDigit . snd) dropped

adj :: Int -> [Int]
adj row = [row-1,row,row+1]

-- 2
gearRatios :: [(Int, [(Int, Char)])] -> [Int]
gearRatios rows = map gearRatio $ filter isGear all_stars
    where all_stars = concat $ map (\x -> zip (repeat $ fst x) (stars $ snd x)) rows
          parts = allParts rows
          rel_parts row = filter (\ x -> (r x) `elem` (adj row)) parts
          isGear :: (Int, Int) -> Bool
          isGear (row, i) = length (filter (\x -> check (is x) [i]) $ rel_parts row) == 2
          gearRatio :: (Int, Int) -> Int
          gearRatio (row, i) = product $ map v (filter (\x -> check (is x) [i]) $ rel_parts row)

stars :: [(Int, Char)] -> [Int]
stars = map fst . filter (\x -> (snd x) == '*')