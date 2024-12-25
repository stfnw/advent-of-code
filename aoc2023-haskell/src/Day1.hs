-- https://adventofcode.com/2023/day/1

module Day1 where

import Data.Char (digitToInt, isDigit)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day1.txt"
    putStrLn $ "Day1: " ++ show (run' contents)

run' :: String -> Int
run' contents = sum $ map processLine $ lines contents

processLine :: [Char] -> Int
processLine line =
    let digits = map digitToInt $ filter isDigit line
        num = (\xs -> (head xs * 10) + (last xs)) digits
     in num
