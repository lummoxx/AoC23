module Dec_1 where

import Data.Char (isDigit, digitToInt)
import Data.List (isInfixOf, elemIndices)

main :: IO ()
main = do
    contents <- readFile "1st.txt"
    let nums = map firstAndLastNumber (words contents)
    print $ sum nums

-- 1
firstAndLastDigits :: String -> Int
firstAndLastDigits str = read (firstDigit str : [firstDigit (reverse str)])

firstDigit :: String -> Char
firstDigit str = head $ dropWhile (not . isDigit) str

-- 2
numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

firstAndLastNumber :: String -> Int
firstAndLastNumber str = read $ show (findNumber str numbers) ++ show (findNumber (reverse str) (map reverse numbers))

findNumber :: String -> [String] -> Int
findNumber str ls
    | any (`isInfixOf` letters) ls = findWord letters ls
    | otherwise = digitToInt $ firstDigit str
        where letters = takeWhile (not . isDigit) str

findWord :: String -> [String] -> Int
findWord str ls = head integers
    where indices = map (`elemIndices` ls) (ffs str ls 3)
          integers = map (1 +) $ concat indices

ffs :: String -> [String] -> Int -> [String]
ffs str ls i
    | i > length str + 1 = []
    | any (`isInfixOf` take i str) ls = firstfound
    | otherwise = ffs str ls (i+1)
 where word = take i str
       firstfound = filter (`isInfixOf` word) ls

