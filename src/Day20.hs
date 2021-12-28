{-# LANGUAGE ViewPatterns #-}

module Day20
  ( solution
  , part1
  , part2
  )
where

import qualified GHC.Arr  as A
import           Utils

type Pattern = A.Array Int Bool
type Matrix = A.Array (Int, Int) Bool

type Input = (Pattern, Matrix)

solution :: Solution
solution = (part1, part2, "day20")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 i@(pattern, matrix) = length $ filter id $ A.elems m
  where
  ((_, m),_) = foldl (\state _ -> nextInput state) (i, '0') [1..2]

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 i@(pattern, matrix) = length $ filter id $ A.elems m
  where
  ((_, m),_) = foldl (\state _ -> nextInput state) (i, '0') [1..50]

parseInput :: String -> Input
parseInput (splitOn [] . lines -> [[pat], rmat]) = (pattern, matrix)
  where
  patternLength = length pat
  pattern = A.array (0, patternLength - 1) $ zip [0..] ((== '#') <$> pat)
  maxy = length rmat
  maxx = length $ head rmat
  matrix = A.array ((0,0), (maxx - 1, maxy - 1)) [((x,y), v == '#') | (y, line) <- zip [0..] rmat, (x, v) <- zip [0..] line ]
parseInput _ = error "invalid input"

nextInput :: (Input, Char) -> (Input, Char)
nextInput ((pattern, matrix), inf) = ((pattern, newMatrix), ninf)
  where
  ((minx, miny), (maxx, maxy)) = A.bounds matrix
  nminx = minx - 3
  nminy = miny - 3
  nmaxx = maxx + 3
  nmaxy = maxy + 3
  ninf = if pattern A.! toDec (replicate 9 inf) then '1' else '0'
  newMatrix = A.array ((nminx, nminy), (nmaxx, nmaxy)) [ ((x,y), next (x,y)) | x <- [nminx..nmaxx], y <- [nminy..nmaxy] ]
  next (x,y) = pattern A.! toDec [get (x + dx, y + dy) | dy <- [-1, 0, 1], dx <- [-1, 0, 1]]
  get (x,y)
    | x < minx || x > maxx || y < miny || y > maxy = inf
    | otherwise = if matrix A.! (x, y) then '1' else '0'
