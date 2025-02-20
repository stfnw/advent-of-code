-- https://adventofcode.com/2022/day/2

module Day2 where

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2022/Day2.txt"
    putStrLn $ "Day2 part1: " ++ show (runPart1 contents)
    putStrLn $ "Day2 part2: " ++ show (runPart2 contents)

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

score1 :: Game -> Int
{- ORMOLU_DISABLE -}
score1 (Rock,     Rock)      = 3 + 1
score1 (Rock,     Paper)     = 6 + 2
score1 (Rock,     Scissors)  = 0 + 3
score1 (Paper,    Rock)      = 0 + 1
score1 (Paper,    Paper)     = 3 + 2
score1 (Paper,    Scissors)  = 6 + 3
score1 (Scissors, Rock)      = 6 + 1
score1 (Scissors, Paper)     = 0 + 2
score1 (Scissors, Scissors)  = 3 + 3
{- ORMOLU_ENABLE -}

runPart1 :: String -> Int
runPart1 = sum . map (score1 . readGame) . lines

-- Note: this repurposes the second tuple field without defining duplicated
-- data types. (X = Rock = Lose, Y = Paper = Draw, Z = Scissors = Win)
data PlayRes = Lose | Draw | Win deriving (Show)
playToRes :: Play -> PlayRes
{- ORMOLU_DISABLE -}
playToRes Rock      = Lose
playToRes Paper     = Draw
playToRes Scissors  = Win
{- ORMOLU_ENABLE -}

score2 :: Game -> Int
score2 (p1, p2) = score2' p1 $ playToRes p2

score2' :: Play -> PlayRes -> Int
{- ORMOLU_DISABLE -}
score2' Rock     Lose = score1 (Rock,     Scissors)
score2' Rock     Draw = score1 (Rock,     Rock)
score2' Rock     Win  = score1 (Rock,     Paper)
score2' Paper    Lose = score1 (Paper,    Rock)
score2' Paper    Draw = score1 (Paper,    Paper)
score2' Paper    Win  = score1 (Paper,    Scissors)
score2' Scissors Lose = score1 (Scissors, Paper)
score2' Scissors Draw = score1 (Scissors, Scissors)
score2' Scissors Win  = score1 (Scissors, Rock)
{- ORMOLU_ENABLE -}

runPart2 :: String -> Int
runPart2 = sum . map (score2 . readGame) . lines
