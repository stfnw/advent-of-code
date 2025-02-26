-- Solution to https://adventofcode.com/2022/day/5, with ReadP parsing in mostly monadic style.

module Day5M where

import Data.Char (isAsciiUpper, isDigit)
import Data.List (transpose)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

run :: IO ()
run = do
  content <- readFile "../../advent-of-code-data/aoc2022/Day5.txt"
  putStrLn $ "Day5M part1: " ++ (show . fromJust) (parseInput content >>= processInput)

type Rearrangement = ([Stack], [Move])

type Stack = [Char]

type Move = (Int, Int, Int)

parseInput :: String -> Maybe Rearrangement
parseInput content = case readP_to_S parseRearrangement content of
  [(r, "")] -> Just r
  _ -> Nothing

parseRearrangement :: ReadP Rearrangement
parseRearrangement = do
  stacks <- parseStacks
  _ <- char '\n'
  moves <- many1 parseMove
  eof
  return (stacks, moves)

parseStacks :: ReadP [Stack]
parseStacks = do
  stackst <- many1 parseStackLineBox
  _ <- parseStackLineIndex
  return $ map (filter (/= ' ')) $ transpose stackst

parseStackLine :: (Char -> Bool) -> ReadP [Char]
parseStackLine f = do
  l <-
    sepBy1
      ( do
          _ <- get
          c <- satisfy f
          _ <- get
          return c
      )
      (char ' ')
  _ <- char '\n'
  return l

parseStackLineBox :: ReadP [Char]
parseStackLineBox = parseStackLine $ \c -> isAsciiUpper c || c == ' '

parseStackLineIndex :: ReadP [Char]
parseStackLineIndex = parseStackLine isDigit

parseMove :: ReadP Move
parseMove = do
  _ <- string "move"
  _ <- char ' '
  n <- parseInt
  _ <- char ' '
  _ <- string "from"
  _ <- char ' '
  f <- parseInt
  _ <- char ' '
  _ <- string "to"
  _ <- char ' '
  t <- parseInt
  _ <- char '\n'
  return (n, f - 1, t - 1)

parseInt :: ReadP Int
parseInt = do
  s <- munch1 isDigit
  maybe pfail return (readMaybe s :: Maybe Int)

processInput :: Rearrangement -> Maybe String
processInput r = do
  r' <- processInput' r
  Just $ map head r'

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
