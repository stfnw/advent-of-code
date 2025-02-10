-- https://adventofcode.com/2022/day/1

module Day1 where

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2022/Day1.txt"
    putStrLn $ "Day1: " ++ show (run' contents)

run' :: String -> Int
run' contents =
    maximum $
        map
            (sum . map (read :: String -> Int))
            (foldl foldF [] $ lines contents)
  where
    foldF :: [[String]] -> String -> [[String]]
    foldF [] _ = [[]]
    foldF acc [] = [] : acc
    foldF (a : as) x = (x : a) : as
