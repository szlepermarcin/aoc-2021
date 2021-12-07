module Day7
  ( solution
  , part1
  , part2
  )
where

import           Utils
import Data.List

solution :: Solution
solution = (part1, part2, "day7")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: [Int] -> Int
processPart1 initial = minimum [sum $ fmap (\i -> abs (x - i)) initial | x <- initial]

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: [Int] -> Int
processPart2 initial = minimum [sum $ fmap (\i -> sum [0..(abs (x - i))]) initial | x <- initial]

parseInput :: String -> [Int]
parseInput s = read <$> splitOn ',' s
