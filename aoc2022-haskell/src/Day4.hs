-- https://adventofcode.com/2022/day/4

module Day4 where

run :: IO ()
run = do
  contents <- readFile "../../advent-of-code-data/aoc2022/Day4.txt"
  putStrLn $ "Day4 part1: " ++ processInput isFullyOverlapping contents
  putStrLn $ "Day4 part2: " ++ processInput isPartiallyOverlapping contents

processInput :: ([[Int]] -> Bool) -> String -> String
processInput f = show . length . filter id . map (f . pair) . lines

pair :: String -> [[Int]]
pair = map (map read . words . replace '-' ' ') . words . replace ',' ' '

replace :: Char -> Char -> String -> String
replace c d = map (\a -> if a == c then d else a)

isFullyOverlapping :: [[Int]] -> Bool
isFullyOverlapping [[a1, a2], [b1, b2]] =
  (a1 <= b1 && b2 <= a2)
    || (b1 <= a1 && a2 <= b2)
isFullyOverlapping _ = error "not supposed to happen"

isPartiallyOverlapping :: [[Int]] -> Bool
isPartiallyOverlapping [[a1, a2], [b1, b2]] = not (a2 < b1 || b2 < a1)
isPartiallyOverlapping _ = error "not supposed to happen"
