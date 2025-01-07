-- https://adventofcode.com/2023/day/9

module Day9 where

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day9.txt"
    putStrLn $ "Day9: " ++ show (run' contents)

run' :: [Char] -> Int
run' contents =
    let input = parseInput contents
     in processInput input

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

processInput :: [[Int]] -> Int
processInput histories = sum $ map processHistory histories

processHistory :: [Int] -> Int
processHistory history =
    let diffed = repDiff history
     in sum $ reverse . map last $ diffed

repDiff :: [Int] -> [[Int]]
repDiff lst
    | all (== 0) lst = [lst]
    | otherwise = lst : repDiff (diff lst)

diff :: [Int] -> [Int]
diff lst = zipWith (-) (tail lst) lst
