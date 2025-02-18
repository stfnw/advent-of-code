-- https://adventofcode.com/2022/day/1

module Day1 where

import Data.List (sortBy)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2022/Day1.txt"
    putStrLn $ "Day1 part1: " ++ show (runPart1 contents)
    putStrLn $ "Day1 part2: " ++ show (runPart2 contents)

runPart1 :: String -> Int
runPart1 = runTopElves 1

runPart2 :: String -> Int
runPart2 = runTopElves 3

runTopElves :: Int -> String -> Int
runTopElves ntop contents =
    sum $ take ntop
        $ sortBy (flip compare)
        $ map (sum . map read)
        $ groupLinesInParagraph contents

groupLinesInParagraph :: String -> [[String]]
groupLinesInParagraph = foldl foldF [] . lines
  where
    foldF :: [[String]] -> String -> [[String]]
    foldF [] _ = [[]]
    foldF acc [] = [] : acc
    foldF (a : as) x = (x : a) : as
