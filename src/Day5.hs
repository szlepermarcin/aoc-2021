{-# LANGUAGE TupleSections #-}
module Day5
  ( solution
  , part1
  , part2
  )
where

import           Data.List
import           Utils

type Point = (Int, Int)

invalidPoint :: Point
invalidPoint = (-1, -1)

solution :: Solution
solution = (part1, part2, "day5")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: [(Point, Point)] -> Int
processPart1 l = process [] (filter isHorizontalVerticalLine l)
  where
  process x []       = length $ filter (\v -> length v > 1 ) (group $ sort x)
  process acc (x:xs) = process (acc ++ getLinePoints x) xs

part2 :: String -> String
part2 = show . processPart2 . parseInput


processPart2 :: [(Point, Point)] -> Int
processPart2 l = process [] (filter (\v -> isHorizontalVerticalLine v || isDiagonalLine v) l)
  where
  process x []       = length $ filter (\v -> length v > 1 ) (group $ sort x)
  process acc (x:xs) = process (acc ++ getLinePoints x) xs

parseInput :: String -> [(Point, Point)]
parseInput s = fmap parseLine (lines s)

parseLine :: String -> (Point, Point)
parseLine x = case splitOn ' ' x of
  [p1, "->", p2] -> (parsePoint p1, parsePoint p2)
  _              -> (invalidPoint, invalidPoint)

parsePoint :: String -> Point
parsePoint x = case splitOn ',' x of
  [n1, n2] -> (read n1, read n2)
  _        -> invalidPoint

isHorizontalVerticalLine :: (Point, Point) -> Bool
isHorizontalVerticalLine ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

isDiagonalLine :: (Point, Point) -> Bool
isDiagonalLine ((x1, y1), (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

getLinePoints :: (Point, Point) -> [Point]
getLinePoints ((x1, y1), (x2, y2))
  | x1 == x2 = fmap (x1,) (mkseq y1 y2)
  | y1 == y2 = fmap (,y1) (mkseq x1 x2)
  | abs (x1 - x2) == abs (y1 - y2) = zip (mkseq x1 x2) (mkseq y1 y2)
  | otherwise = []
  where
  mkseq i1 i2
    | i1 < i2 = [i1..i2]
    | otherwise = [i1,(i1-1)..i2]
