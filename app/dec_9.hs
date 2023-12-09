module Dec_9 where
import Data.List.Extra (allSame)

day9 :: IO ()
day9 = do
    ls <- map (map read . words) . lines <$> readFile "9th.txt"
    print $ sum $ map (extrapolate (+) (\xs -> zipWith (-) xs (tail xs)) . reverse) ls
    print $ sum $ map (extrapolate (-) (\xs -> zipWith (-) (tail xs) xs)) ls

extrapolate :: (Int -> Int -> Int) -> ([Int] -> [Int]) -> [Int] -> Int
extrapolate f g = foldr f 0 . go
    where go ls
            | allSame ls = [head ls]
            | otherwise = head ls : go (g ls)