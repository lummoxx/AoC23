module Dec_7 where
import Data.Char (isLetter)
import Data.Maybe
import Data.List

day7 :: IO ()
day7 = do
    contents <- readFile "7th.txt"
    print $ sum $ zipWith (\x y -> x * (bid y)) [1..] $ sort $ map parseHand $ lines contents

data Hand = H {htype :: Int, points :: [Int], bid :: Int}

instance Eq Hand where
    (H t  p _) == (H t' p' _) = (t:p) == (t':p')

instance Ord Hand where
    (H t  p _) `compare` (H t' p' _) = (t:p) `compare` (t':p')

parseHand :: String -> Hand
parseHand hand = H (handType val) (map value val) (read bid)
    where (val,bid) = (\x->(x !! 0, x !! 1)) $ words hand

handType :: String -> Int
handType hand
    | length no_jokes == 1 = 7
    | length no_jokes == 2 =
        if any (\x -> jokes + (length $ filter (== x) hand) == 4) no_jokes then 6 else 5
    | length no_jokes == 3 =
        if any (\x -> jokes + (length $ filter (== x) hand) == 3) no_jokes then 4 else 3
    | length no_jokes == 4 = 2
    | length no_jokes == 5 = 1
    | otherwise = 7
        where no_jokes = filter (/= 'J') $ nub hand
              jokes = length $ filter (== 'J') hand

value :: Char -> Int
value c
    | c == 'J' = 1
    | isLetter c = 10 + (fromJust $ elemIndex c "TJQKA")
    | otherwise = read [c]