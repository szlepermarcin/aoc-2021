module Day2
  ( solution
  , part1
  , part2
  )
where

import           Utils

solution :: (Solution, Solution, String)
solution = (part1, part2, "day2")

part1 :: String -> String
part1 = (show . processPart1) . map parseCommand . lines
  where
    parseCommand line = case splitOn ' ' line of [pos, value] -> (pos, read value)

processPart1 :: [(String, Int)] -> Int
processPart1 = calc 0 0
  where
    calc horiz depth []                    = horiz * depth
    calc horiz depth (("forward", v) : xs) = calc (horiz + v) depth xs
    calc horiz depth (("down", v) : xs)    = calc horiz (depth + v) xs
    calc horiz depth (("up", v) : xs)      = calc horiz (depth - v) xs

part2 :: String -> String
part2 = (show . processPart2) . map parseCommand . lines
  where
    parseCommand line = case splitOn ' ' line of [pos, value] -> (pos, read value)

processPart2 :: [(String, Int)] -> Int
processPart2 = calc 0 0 0
  where
    calc horiz depth aim [] = horiz * depth
    calc horiz depth aim (("forward", v) : xs) = calc (horiz + v) (depth + (v * aim)) aim xs
    calc horiz depth aim (("down", v) : xs) = calc horiz depth (aim + v) xs
    calc horiz depth aim (("up", v) : xs) = calc horiz depth (aim - v) xs
