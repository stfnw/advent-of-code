-- https://adventofcode.com/2023/day/4

module Day4 where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, intersection, size)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day4.txt"
    putStrLn $ "Day4: " ++ show (run' contents)

run' :: String -> Int
run' s =
    let res = sum <$> traverse processCard (lines s)
     in fromMaybe (error "") res

processCard :: [Char] -> Maybe Int
processCard s = case splitOn ": " s of
    [_, t] -> processCard' t
    _ -> Nothing

processCard' :: [Char] -> Maybe Int
processCard' s = case splitOn " | " s of
    [goal, act] -> Just $ processCard'' goal act
    _ -> Nothing

processCard'' :: String -> String -> Int
processCard'' goal act =
    let goal' = toIntSet goal
        act' = toIntSet act
        winning = intersection goal' act'
        n = size winning
     in if n == 0 then 0 else 2 ^ (n - 1)

toIntSet :: String -> Set Int
toIntSet = fromList . map read . words
