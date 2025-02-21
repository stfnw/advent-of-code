-- https://adventofcode.com/2022/day/3

module Day3 where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Word (Word64)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2022/Day3.txt"
    putStrLn $ "Day3 part1: " ++ show (runPart1 contents)

-- Use u64 as a hash-map for the <64 lower and uppercase letters that we need
-- to keep track of.
newtype Compartment = Compartment {getCompartment :: Word64} deriving (Show)

getIndex :: Char -> Int
getIndex c
    | ord 'a' <= ord c && ord c <= ord 'z' = ord c - ord 'a'
    | ord 'A' <= ord c && ord c <= ord 'Z' = ord c - ord 'A' + 26
    | otherwise = error ("Unexpected char " ++ show c)

toChar :: Int -> Char
toChar i
    | 0 * 26 <= i && i < 1 * 26 = chr $ i + ord 'a'
    | 1 * 26 <= i && i < 2 * 26 = chr $ i + ord 'A' + 26
    | otherwise = error ("Unexpected char " ++ show i)

getPriorityFromIndex :: Int -> Int
getPriorityFromIndex i = i + 1

getPriorities :: Compartment -> [Int]
getPriorities (Compartment c) =
    [0 .. 26 * 2]
        & filter (\i -> c .&. shiftL 1 i /= 0)
        & map getPriorityFromIndex

parseCompartment :: String -> Compartment
parseCompartment =
    Compartment
        . foldl (\acc c -> acc .|. (shiftL 1 . getIndex $ c)) 0

intersection :: Compartment -> Compartment -> Compartment
intersection (Compartment a) (Compartment b) = Compartment (a .&. b)

-- https://stackoverflow.com/questions/8529814/get-a-sublist-in-haskell
slice :: Int -> Int -> [a] -> [a]
slice from to = take (to - from) . drop from

splitHalf :: String -> (Compartment, Compartment)
splitHalf s =
    let len = length s
        half = len `div` 2
     in (parseCompartment $ slice 0 half s, parseCompartment $ slice half len s)

runPart1 :: String -> Int
runPart1 content =
    lines content
        & map (getPriorities . uncurry intersection . splitHalf)
        & mconcat
        & sum
