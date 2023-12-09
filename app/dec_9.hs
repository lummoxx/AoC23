module Dec_9 where

day9 :: IO ()
day9 = do
    ls <- map (map read . words) . lines <$> readFile "9th.txt"
    print $ sum $ map (extrapolate (+) (\xs -> zipWith (-) xs (tail xs)))  (map reverse ls)
    print $ sum $ map (extrapolate (-) (\xs -> zipWith (-) (tail xs) xs)) ls

intermediate :: ([Int] -> [Int]) -> [Int] -> [[Int]]
intermediate f ls
    | all (== head ls) ls = [[head ls]]
    | otherwise = take 2 ls : intermediate f (f ls)

extrapolate :: (Int -> Int -> Int) -> ([Int] -> [Int]) -> [Int] -> Int
extrapolate f g = foldr f 0 . map head . intermediate g