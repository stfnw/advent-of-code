-- https://adventofcode.com/2023/day/2

module Day2 where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day2.txt"
    putStrLn $ "Day2: " ++ show (run' contents)

data Game = Game {gameid :: Int, cubes :: [Cube]} deriving (Show)

data Cube = Cube {red :: Int, green :: Int, blue :: Int} deriving (Show)

instance Semigroup Cube where
    Cube r1 g1 b1 <> Cube r2 g2 b2 = Cube (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid Cube where
    mempty = Cube 0 0 0

run' :: String -> Int
run' s =
    let games = traverse parseGame $ lines s
        validGames = filter isValidGame <$> games
        validIds = map gameid <$> validGames
     in maybe (error "Invalid input") sum validIds

-- e.g.: Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
parseGame :: String -> Maybe Game
parseGame line = case splitOn ": " line of
    [g, c] ->
        let gameId = readMaybe . last $ words g
         in Game <$> gameId <*> parseCubes c
    _ -> Nothing

-- e.g.: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
parseCubes :: String -> Maybe [Cube]
parseCubes s =
    let cs = splitOn "; " s
     in traverse parseCube cs

-- e.g.: 3 blue, 4 red
parseCube :: String -> Maybe Cube
parseCube s =
    let entries = splitOn ", " s
     in mconcat <$> traverse parseEntry entries

-- e.g.: 3 blue
parseEntry :: String -> Maybe Cube
parseEntry entry = case words entry of
    [n, c] -> createCube c =<< readMaybe n
    _ -> Nothing

createCube :: String -> Int -> Maybe Cube
createCube "red" n = Just $ Cube n 0 0
createCube "green" n = Just $ Cube 0 n 0
createCube "blue" n = Just $ Cube 0 0 n
createCube _ _ = Nothing

isValidGame :: Game -> Bool
isValidGame game = all isValidCube $ cubes game

-- 12 red cubes, 13 green cubes, and 14 blue cube
isValidCube :: Cube -> Bool
isValidCube = isValidCube' 12 13 14

isValidCube' :: Int -> Int -> Int -> Cube -> Bool
isValidCube' r g b c = red c <= r && green c <= g && blue c <= b
