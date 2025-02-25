-- Solution to https://adventofcode.com/2022/day/5, with ReadP parsing in mostly applicative style.

module Day5A where

import Data.Char (isAsciiUpper, isDigit)
import Data.List (transpose)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

run :: IO ()
run = do
  content <- readFile "../../advent-of-code-data/aoc2022/Day5.txt"
  putStrLn $ "Day5A part1: " ++ (show . fromJust) (parseInput content >>= processInput)

type Rearrangement = ([Stack], [Move])

type Stack = [Char]

type Move = (Int, Int, Int)

parseInput :: String -> Maybe Rearrangement
parseInput content = case readP_to_S parseRearrangement content of
  [(r, "")] -> Just r
  _ -> Nothing

parseRearrangement :: ReadP Rearrangement
parseRearrangement =
  (,)
    <$> (parseStacks <* char '\n')
    <*> (parseMoves <* eof)

parseStacks :: ReadP [Stack]
parseStacks =
  map (filter (/= ' ')) . transpose
    <$> (many1 parseStackLineBox <* parseStackLineIndex)

parseStackLine :: (Char -> Bool) -> ReadP [Char]
parseStackLine f = sepBy1 (get *> satisfy f <* get) (char ' ') <* char '\n'

parseStackLineBox :: ReadP [Char]
parseStackLineBox = parseStackLine $ \c -> isAsciiUpper c || c == ' '

parseStackLineIndex :: ReadP [Char]
parseStackLineIndex = parseStackLine isDigit

parseMoves :: ReadP [Move]
parseMoves = many1 parseMove

parseMove :: ReadP Move
parseMove =
  (,,)
    <$> (string "move" *> char ' ' *> parseInt)
    <*> (char ' ' *> string "from" *> char ' ' *> ((\i -> i - 1) <$> parseInt))
    <*> (char ' ' *> string "to" *> char ' ' *> ((\i -> i - 1) <$> parseInt) <* char '\n')

parseInt :: ReadP Int
parseInt = do
  s <- munch1 isDigit
  maybe pfail return (readMaybe s :: Maybe Int)

processInput :: Rearrangement -> Maybe String
processInput r = map head <$> processInput' r

processInput' :: Rearrangement -> Maybe [Stack]
processInput' (s, []) = Just s
processInput' (s, (0, _, _) : ms) = processInput' (s, ms)
processInput' (s, (n, f, t) : ms) =
  let sf = tail $ s !! f
      st = head (s !! f) : (s !! t)
      r = (replaceNth t st $ replaceNth f sf s, (n - 1, f, t) : ms)
   in processInput' r

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n new xs
  | n < 0 || n >= length xs = xs
  | otherwise = take n xs ++ [new] ++ drop (n + 1) xs
