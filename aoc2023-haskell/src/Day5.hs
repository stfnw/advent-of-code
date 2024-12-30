-- https://adventofcode.com/2023/day/5

module Day5 where

import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

type Input = ([Int], [Dict])
data Dict = Dict {dfrom :: String, dto :: String, ranges :: [Range]} deriving (Show)
data Range = Range {dstStart :: Int, srcStart :: Int, dlen :: Int} deriving (Show)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day5.txt"
    putStrLn $ "Day5: " ++ show (run' contents)

run' :: String -> Int
run' s =
    let (seeds, dicts) = parseInput s
        seeds' = parseSeeds seeds
        dicts' = traverse parseDict dicts
        vals = processDicts <$> seeds' <*> dicts'
     in maybe (error "Invalid input") minimum vals

parseInput :: String -> ([String], [[String]])
parseInput s =
    let
        step :: ([[String]], [String]) -> String -> ([[String]], [String])
        step (acc, tmpacc) line
            | null line = (tmpacc : acc, [])
            | otherwise = (acc, line : tmpacc)

        (allbutlast, thelast) = foldl step ([], []) $ lines s
        parsed = map reverse . reverse $ thelast : allbutlast
     in
        (head parsed, tail parsed)

parseSeeds :: [String] -> Maybe [Int]
parseSeeds [x]
    | "seeds:" `isInfixOf` x =
        Just $ map read $ drop 1 $ words x
parseSeeds _ = Nothing

parseDict :: [String] -> Maybe Dict
parseDict (x : xs) | "-to-" `isInfixOf` x =
    case splitOn "-to-" . head $ words x of
        [f, t] -> Dict f t <$> traverse parseRange xs
        _ -> Nothing
parseDict _ = Nothing

parseRange :: String -> Maybe Range
parseRange lst = case toIntList lst of
    [a, b, c] -> Just $ Range a b c
    _ -> Nothing

toIntList :: String -> [Int]
toIntList s = map read $ words s

processDicts :: [Int] -> [Dict] -> [Int]
processDicts =
    let
        f :: [Int] -> Dict -> [Int]
        f vs d = map (`translateVal` d) vs
     in
        foldl f

translateVal :: Int -> Dict -> Int
translateVal val dict = fromMaybe val $ listToMaybe $ mapMaybe (translateVal' val) (ranges dict)

translateVal' :: Int -> Range -> Maybe Int
translateVal' val r
    | srcStart r <= val && val < srcStart r + dlen r =
        Just $ dstStart r + (val - srcStart r)
    | otherwise =
        Nothing
