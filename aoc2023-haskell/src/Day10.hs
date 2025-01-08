-- https://adventofcode.com/2023/day/10

module Day10 where

import Control.Exception (assert)
import Control.Monad (guard)
import Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as UArray
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Tuple (swap)

data Grid = Grid
    { ggrid :: UArray.UArray Int Char
    , gcols :: Int
    , grows :: Int
    }
    deriving (Show)

data Pos = Pos {posx :: Int, posy :: Int} deriving (Eq, Ord, Show)

data Dir = DUp | DDown | DLeft | DRight deriving (Show)

run :: IO ()
run = do
    contents <- readFile "../../advent-of-code-data/aoc2023/Day10.txt"
    putStrLn $ "Day10: " ++ show (run' contents)

run' :: String -> Int
run' contents =
    let (cols, rows) = (length . head $ lines contents, length $ lines contents)
        tmp' = filter (/= '\n') contents
        tmp = assert (length tmp' == cols * rows) tmp'
        grid =
            Grid
                (UArray.listArray (0, length tmp - 1) tmp)
                cols
                rows
        start_idx = fromMaybe (error "S not found") $ elemIndex 'S' tmp
        pos = uncurry Pos $ swap (start_idx `divMod` cols)
        visited = Set.empty
        visited' = dfs grid pos visited
     in (Set.size visited' + 1) `div` 2

-- Note: there is only one valid loop => we don't need backtracking.
dfs :: Grid -> Pos -> Set.Set Pos -> Set.Set Pos
dfs grid pos visited =
    foldl visitNeighbor (Set.insert pos visited) [DUp, DDown, DLeft, DRight]
  where
    visitNeighbor :: Set.Set Pos -> Dir -> Set.Set Pos
    visitNeighbor vis dir =
        if canGo grid vis pos dir
            then
                let pos' = fromMaybe (error "") $ addPosDir pos dir
                    vis' = Set.insert pos' vis
                 in dfs grid pos' vis'
            else vis

-- This implementation uses `Maybe ()` as kind of pseudo-bool in order
-- to be able to use monadic composition with do-notation / bind for
-- sequential composition for early returns without much nesting.
{- ORMOLU_DISABLE -}
canGo :: Grid -> Set.Set Pos -> Pos -> Dir -> Bool
canGo grid visited pos dir =
  let res = do
        pos' <- addPosDir pos dir
        guard $ not $ Set.member pos' visited
        c <- getGrid pos grid
        c' <- getGrid pos' grid
        case (c, dir, c') of
          ( '.',  _,       _   ) -> Nothing
          ( _,    _,       '.' ) -> Nothing
          ( '|',  DUp,     _   ) -> Just ()
          ( '|',  DDown,   _   ) -> Just ()
          ( '-',  DLeft,   _   ) -> Just ()
          ( '-',  DRight,  _   ) -> Just ()
          ( 'L',  DUp,     _   ) -> Just ()
          ( 'L',  DRight,  _   ) -> Just ()
          ( 'J',  DUp,     _   ) -> Just ()
          ( 'J',  DLeft,   _   ) -> Just ()
          ( '7',  DDown,   _   ) -> Just ()
          ( '7',  DLeft,   _   ) -> Just ()
          ( 'F',  DDown,   _   ) -> Just ()
          ( 'F',  DRight,  _   ) -> Just ()
          ( 'S',  DUp,     _   ) -> Just ()
          ( 'S',  DDown,   _   ) -> Just ()
          ( 'S',  DLeft,   _   ) -> Just ()
          ( 'S',  DRight,  _   ) -> Just ()
          _                      -> Nothing
   in isJust res
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
addPosDir :: Pos -> Dir -> Maybe Pos
addPosDir (Pos x y) dir =
  let (x', y') = case dir of
        DUp    -> (x,     y - 1)
        DDown  -> (x,     y + 1)
        DLeft  -> (x - 1, y    )
        DRight -> (x + 1, y    )
   in if 0 <= x' && 0 <= y'
        then Just $ Pos x' y'
        else Nothing
{- ORMOLU_ENABLE -}

getGrid :: Pos -> Grid -> Maybe Char
getGrid (Pos x y) (Grid grid cols rows)
    | x < cols && y < rows = Just $ grid ! (y * cols + x)
    | otherwise = Nothing

-- ugly nested version that is equivalent to the one above
{-
canGo' :: Grid -> Set.Set Pos -> Pos -> Dir -> Bool
canGo' grid visited pos dir =
  case addPosDir pos dir of
    Nothing -> False
    Just pos' ->
      not (Set.member pos' visited)
        && case getGrid pos grid of
          Nothing -> False
          Just c ->
            case getGrid pos' grid of
              Nothing -> False
              Just c' ->
                case (c, dir, c') of
                  ('.', _,      _  ) -> False
                  (_,   _,      '.') -> False
                  ('|', DUp,    _  ) -> True
                  ('|', DDown,  _  ) -> True
                  ('-', DLeft,  _  ) -> True
                  ('-', DRight, _  ) -> True
                  ('L', DUp,    _  ) -> True
                  ('L', DRight, _  ) -> True
                  ('J', DUp,    _  ) -> True
                  ('J', DLeft,  _  ) -> True
                  ('7', DDown,  _  ) -> True
                  ('7', DLeft,  _  ) -> True
                  ('F', DDown,  _  ) -> True
                  ('F', DRight, _  ) -> True
                  ('S', DUp,    _  ) -> True
                  ('S', DDown,  _  ) -> True
                  ('S', DLeft,  _  ) -> True
                  ('S', DRight, _  ) -> True
                  _ -> False
-}
