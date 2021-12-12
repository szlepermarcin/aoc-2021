module Day11
  ( solution
  , part1
  , part2
  )
where

import           Data.List
import           Utils

type Coords = (Int, Int)
type Point = (Coords, Int)
type Input = [(Coords, Int)]

solution :: Solution
solution = (part1, part2, "day11")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 i = fst $ foldr (const (resolve . nextDay)) (0, i) [1..100]

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 i = fst $ head $ dropWhile (\(_, (_, state)) -> not . all (==0) $ snd <$> state) $ zip [0..] (iterate (resolve . nextDay) (0, i))

parseInput :: String -> Input
parseInput s = [((x,y), read [v]) | (y, line) <- zip [0..] (lines s), (x, v) <- zip [0..] line]

nextDay :: (Int, Input) -> (Int, Input)
nextDay (acc, i) = (acc, (\(c,v) -> (c, v + 1)) <$> i)

resolve:: (Int, Input) -> (Int, Input)
resolve x@(acc, i)
  | isResolved = x
  | otherwise = resolve (newAcc, newInput)
  where
  isResolved = all ((<=9) . snd) i
  flashing = filter ((>9) . snd) i
  newAcc = acc + length flashing
  affected = concat $ adjacents <$> flashing
  adjacents ((x,y), _) = [( x + dx, y + dy) |
     dx <- [- 1, 0, 1], dy <- [- 1, 0, 1]]
  withFlashing = foldr (\ c i -> set i c 0) i (fst <$> flashing)
  newInput = foldr (\ c i -> modify i c (\v -> if v == 0 then 0 else v + 1)) withFlashing affected
