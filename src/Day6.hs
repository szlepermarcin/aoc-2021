module Day6
  ( solution
  , part1
  , part2
  )
where

import           Utils

type FishState = [Int]

solution :: Solution
solution = (part1, part2, "day6")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: FishState -> Int
processPart1 initial = dayValues initial !! 80

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: FishState -> Int
processPart2 initial = dayValues initial !! 256

parseInput :: String -> FishState
parseInput s = extractState $ read <$> splitOn ',' s

extractState :: [Int] -> FishState
extractState fishes =  fmap (\i -> length $ filter (== i) fishes) [0..8]

transition :: FishState -> FishState
transition [f0, f1, f2, f3, f4, f5, f6, f7, f8] = [f1, f2, f3, f4, f5, f6, f7 + f0, f8, f0]
transition _ = undefined

dayValues :: FishState -> [Int]
dayValues initial = fmap sum (iterate transition initial)
