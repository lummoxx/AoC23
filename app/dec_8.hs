module Dec_8 where
import Data.Char (isLetter)
import qualified Data.Map as M

day8 :: IO ()
day8 = do
    ls <- lines <$> readFile "8th.txt"
    let mapLR =  M.fromList $ map parseLine $ (tail . tail) ls
        a_keys = filter ((== 'A') . last) $ M.keys mapLR
    print $ steps (== "ZZZ") (cycle (head ls)) mapLR 0 "AAA"
    print $ foldr lcm 1 $ map (steps ((== 'Z') . last) (cycle (head ls)) mapLR 0) a_keys

type MapLR = M.Map String (String, String)

parseLine :: String -> (String, (String, String))
parseLine str = (key, (tail v1, take 3 $ dropWhile (not . isLetter) v2))
    where (key, (v1,v2)) = (\(x,y) -> (takeWhile isLetter x, (break (== ',') y))) $ break (== '(') str

steps ::  (String -> Bool) -> String -> MapLR -> Int -> String -> Int
steps f is mapLR s start
    | f res = s'
    | otherwise = steps f (tail is) mapLR s' res
        where (res, s') = go (head is) start s
              go :: Char -> String -> Int -> (String, Int)
              go 'R' from count = (snd $ mapLR M.! from, count+1)
              go _ from count = (fst $ mapLR M.! from, count+1)