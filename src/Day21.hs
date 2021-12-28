{-# LANGUAGE ViewPatterns #-}
module Day21
  ( solution
  , part1
  , part2
  )
where

import qualified Data.Map    as M
import           Debug.Trace
import           Utils

data Game = Game
  { position1  :: Int
  , position2  :: Int
  , score1     :: Int
  , score2     :: Int
  , nextPlayer :: Int
  } deriving (Show, Eq, Ord)

initial :: Int -> Int -> Game
initial p1 p2 = Game p1 p2 0 0 1

type Input = Game


solution :: Solution
solution = (part1, part2, "day21")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 g = min s1 s2 * dc
  where
  (Game p1 p2 s1 s2 np, dc) = play (g, 0)

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 g = uncurry max $ snd $ play2 M.empty g

parseInput :: String -> Input
parseInput (parsed -> [p1, p2]) = initial p1 p2
parseInput _                    = error "invalid input"

parsed s = parseLine . splitOn ' ' <$> lines s

parseLine [_,_,_,_,d] = read d
parseLine _           = error "invalid line"

play :: (Game, Int) -> (Game, Int)
play (g@(Game p1 p2 s1 s2 np), dc)
  | s1 >= 1000 || s2 >= 1000 = (g, dc)
  | np == 1 = play (Game np1 p2 (s1 + np1) s2 2, dc + 3)
  | otherwise = play (Game p1 np2 s1 (s2 + np2) 1, dc + 3)
  where
  ds = sum [ ((dc + m) `mod` 100) + 1 | m <- [0..2]]
  np1 = (((p1 + ds) - 1) `mod` 10) + 1
  np2 = (((p2 + ds) - 1) `mod` 10) + 1

diracRolls :: [(Int, Int)]
diracRolls = (\l@(x:_) -> (x, length l) ) <$> group (sort [x + y + z | x <- [1..3], y <- [1..3], z <- [1..3]])

type Memo = M.Map Game (Int, Int)

play2 :: Memo -> Game -> (Memo, (Int, Int))
play2 m g@(Game p1 p2 s1 s2 np)
  | s1 >= 21 = (m, (1, 0))
  | s2 >= 21 = (m, (0, 1))
  | otherwise = case M.lookup g m of
    Just v -> (m, v)
    _ -> (M.insert g result m', result)
      where
      (m', result) = foldl update (m, (0,0)) subgames
      subgames = first (mkmove g) <$> diracRolls
      mkmove g'@(Game p1' p2' s1' s2' np') ds
        | np == 1 = Game np1 p2 (s1 + np1) s2 2
        | otherwise = Game p1 np2 s1 (s2 + np2) 1
        where
        np1 = (((p1 + ds) - 1) `mod` 10) + 1
        np2 = (((p2 + ds) - 1) `mod` 10) + 1
      update (mm, (r1, r2)) (gg, c) = (mm', (r1 + (c * r1'), r2 + (c * r2')))
        where
        (mm', (r1', r2')) = play2 mm gg
