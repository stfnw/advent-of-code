-- https://adventofcode.com/2023/day/7

module Day7 where

import Control.Exception (assert)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromMaybe)

data Line = Line Hand Bid
    deriving (Show)

type Bid = Int

-- We store two different represantations of the same information:
-- (This allows for easier implementation of the desired comparison operations)
--
--     cards: just a list of the `handLength` cards in the hand,
--         e.g. "32T3K"
--
--     groupedCounts: count the number of occurrences of each card,
--         and then group these counts into `handLength` different bins.
--         e.g. "32T3K" => [3,1,0,0,0]
--         which is read as:
--             - there are 3 instances of cards in the hand occurring exactly 1 time ("2", "T", "K")
--             - there is  1 instance  of cards in the hand occurring exactly 2 times ("3")
--             - there are 0 instances of cards in the hand occurring exactly 3, 4 or 5 times
data Hand = Hand {cards :: [Card], groupedCounts :: [Int]}
    deriving (Eq, Show)

handLength :: Int
handLength = 5

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Show)

newtype Card = Card Char deriving (Eq, Show)

validCards :: [Char]
validCards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day7.txt"
    putStrLn $ "Day7: " ++ show (run' contents)

run' :: [Char] -> Bid
run' contents =
    let input = map parseLine $ lines contents
        sorted = sortBy (\(Line h1 _) (Line h2 _) -> h1 `compare` h2) input
        bids = map (\(Line _ bid) -> bid) sorted
     in sum (zipWith (*) [1 ..] bids)

parseLine :: String -> Line
parseLine line =
    case words line of
        [h, b] -> Line (read h) (read b)
        _ -> error "wrong input"

instance Read Card where
    readsPrec _ (h : rest)
        | h `elem` validCards = [(Card h, rest)]
        | otherwise = []
    readsPrec _ _ = []

instance Ord Card where
    card1 `compare` card2 =
        let crds = Card <$> validCards
            i1 = elemIndex card1 crds
            i2 = elemIndex card2 crds
            res = compare <$> i1 <*> i2
         in fromMaybe (error "Not a card") res

instance Read Hand where
    readsPrec _ s =
        let cardList = map (read . (: [])) s
            cardCounts = countCards cardList
            cardGroups = countCardGroups cardCounts
         in [(Hand cardList cardGroups, "")]

instance Ord Hand where
    h1@(Hand c1 _) `compare` h2@(Hand c2 _) =
        case handType h1 `compare` handType h2 of
            LT -> LT
            GT -> GT
            EQ -> c1 `compare` c2

-- Count number of occurences of each character.
countCards :: [Card] -> [Int]
countCards crds =
    let counts = replicate (length validCards) 0
     in foldl updateCounts counts crds

updateCounts :: [Int] -> Card -> [Int]
updateCounts counts (Card c) =
    let index = fromMaybe (error "Not a card") $ elemIndex c validCards
     in take index counts ++ [counts !! index + 1] ++ drop (index + 1) counts

countCardGroups :: [Int] -> [Int]
countCardGroups counts =
    let idx = [1 .. handLength]
        groups = map (\i -> length . filter (== i) $ counts) idx
     in -- Shuffling around numbers and changing representations/
        -- ways of counting the cards shouldn't change the actual
        -- number of cards present.
        assert
            (sum (zipWith (*) [1 ..] groups) == handLength)
            groups

handType :: Hand -> HandType
handType (Hand _ g) = case g of
    [0, 0, 0, 0, 1] -> FiveOfAKind
    [1, 0, 0, 1, 0] -> FourOfAKind
    [0, 1, 1, 0, 0] -> FullHouse
    [2, 0, 1, 0, 0] -> ThreeOfAKind
    [1, 2, 0, 0, 0] -> TwoPair
    [3, 1, 0, 0, 0] -> OnePair
    [5, 0, 0, 0, 0] -> HighCard
    _ -> error "Not a valid hand"
