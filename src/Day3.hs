module Day3
  ( solution
  , part1
  , part2
  )
where

import           Utils

solution :: Solution
solution = (part1, part2, "day3")

part1 :: String -> String
part1 = (show . processPart1) . lines

processPart1 :: [String] -> Int
processPart1 l = product $ fmap (\f -> toDec $ f $ process l) [fmap common, fmap (common . swap)]

part2 :: String -> String
part2 = (show . processPart2) .  lines

processPart2 :: [String] -> Int
processPart2 l =  commonMatch 0 (==) l * commonMatch 0 (/=) l
  where
  commonMatch _ _ [] = 0
  commonMatch _ _ [x] = toDec x
  commonMatch d method v = commonMatch (d + 1) method (filter (\r -> common (process v !! d) `method` (r !! d)) v)

process = foldr (zipWith (<>)) (repeat mempty) . fmap (fmap extrDigit)
    where
    extrDigit '0' = (Sum 1, Sum 0)
    extrDigit '1' = (Sum 0, Sum 1)
    extrDigit _   = (Sum 0, Sum 0)

common (s1, s2)
    | s1 > s2 = '0'
    | otherwise = '1'
