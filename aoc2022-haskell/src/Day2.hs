-- https://adventofcode.com/2022/day/2

module Day2 where

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2022/Day2.txt"
    putStrLn $ "Day2 part1: " ++ show (runPart1 contents)

type Game = (Play, Play)
data Play = Rock | Paper | Scissors deriving (Show)

readGame :: String -> Game
readGame input =
    case map readPlay . words $ input of
        [a, b] -> (a, b)
        _ -> error "Unexpected input"

readPlay :: String -> Play
readPlay c
    | c == "A" || c == "X" = Rock
    | c == "B" || c == "Y" = Paper
    | c == "C" || c == "Z" = Scissors
    | otherwise = error "Unexpected game choice"

score :: Game -> Int
{- ORMOLU_DISABLE -}
score (Rock,     Rock)      = 3 + 1
score (Rock,     Paper)     = 6 + 2
score (Rock,     Scissors)  = 0 + 3
score (Paper,    Rock)      = 0 + 1
score (Paper,    Paper)     = 3 + 2
score (Paper,    Scissors)  = 6 + 3
score (Scissors, Rock)      = 6 + 1
score (Scissors, Paper)     = 0 + 2
score (Scissors, Scissors)  = 3 + 3
{- ORMOLU_ENABLE -}

runPart1 = sum . map (score . readGame) . lines
