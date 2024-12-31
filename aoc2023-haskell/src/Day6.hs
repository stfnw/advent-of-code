-- https://adventofcode.com/2023/day/6

module Day6 where

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day6.txt"
    putStrLn $ "Day6: " ++ show (run' contents)

data Race = Race {time :: Int, distanceRecord :: Int} deriving (Show)

run' :: [Char] -> Int
run' contents =
    let races = parseInput contents
        wins = map waysToWin <$> races
     in maybe (error "Invalid input") product wins

parseInput :: String -> Maybe [Race]
parseInput s =
    let pairs = case map parseLine $ lines s of
            [h, t] -> Just $ zip h t
            _ -> Nothing
        races = map (uncurry Race) <$> pairs
     in races

parseLine :: String -> [Int]
parseLine line = map read $ drop 1 $ words line

-- "Optimized" version which omits looping through all intermediate values.
-- We realize that `calcDistance` describes a parabola.
-- We try to solve the quadratic equation h*(t-h) > d for h.
-- The two roots give us the low and high cut-offs described here.
waysToWin :: Race -> Int
waysToWin (Race t d) =
    let low = ceiling $ (0.5 :: Double) * (fromIntegral t - sqrt (fromIntegral (t * t - 4 * d))) -- inclusive
        high = floor $ (0.5 :: Double) * (sqrt (fromIntegral (t * t - 4 * d)) + fromIntegral t) -- inclusive
     in (high + 1) - low

-- Naive version: this loops through all valid hold times,
-- calculates the travel distance for each and then checks
-- which hold time leads to a distance larger then the threshold.
waysToWin' :: Race -> Int
waysToWin' (Race t d) =
    let
        -- for 0ms hold time we travel 0mm (velocity always 0)
        -- for t-ms hold time we travel 0mm (velocity maximum, but no time to travel)
        validHoldTimes = [1 .. (t - 1)]
        distances = map (`calcDistance` t) validHoldTimes
        wins = filter (> d) distances
        nWins = length wins
     in
        nWins

calcDistance :: Int -> Int -> Int
calcDistance hold total = hold * (total - hold)
