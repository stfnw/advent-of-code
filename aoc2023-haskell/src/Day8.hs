-- https://adventofcode.com/2023/day/8

module Day8 where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Instruction = L | R deriving (Eq, Show)
type Network = Map.Map String (String, String)
data Input = Input {instructions :: [Instruction], network :: Network}
    deriving (Show)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day8.txt"
    putStrLn $ "Day8: " ++ show (run' contents)

run' :: [Char] -> Int
run' contents =
    let inp = fromMaybe (error "Invalid input") $ parseInput contents
     in processInput (instructions inp) (network inp)

parseInput :: String -> Maybe Input
parseInput contents =
    case lines contents of
        inst : _ : net ->
            Input
                <$> traverse parseInstruction inst
                <*> parseNetwork net
        _ -> Nothing

parseInstruction :: Char -> Maybe Instruction
parseInstruction 'L' = Just L
parseInstruction 'R' = Just R
parseInstruction _ = Nothing

parseNetwork :: [String] -> Maybe Network
parseNetwork net =
    let lst = map parseEdge net
     in Map.fromList <$> sequenceA lst

parseEdge :: String -> Maybe (String, (String, String))
parseEdge edge =
    case words edge of
        [f, "=", l, r] -> Just (f, (init . tail $ l, init r))
        _ -> Nothing

processInput :: [Instruction] -> Network -> Int
processInput insts net = process "AAA" 0 (cycle insts)
  where
    process :: String -> Int -> [Instruction] -> Int
    process "ZZZ" n _ = n
    process s n (inst : rest) =
        let (l, r) =
                fromMaybe (error $ "Invalid key: '" ++ s ++ "' in net " ++ show net) $
                    Map.lookup s net
            s' = case inst of
                L -> l
                R -> r
         in process s' (n + 1) rest
    process _ _ [] = error "End state ZZZ not reached"
