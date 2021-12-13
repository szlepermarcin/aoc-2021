module Day9
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Matrix = [[Int]]
type Coords = (Int, Int)
type Point = (Coords, Int)
type Input = ([(Coords, Int)], Matrix)

solution :: Solution
solution = (part1, part2, "day9")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 i = sum $ (+ 1) . snd <$> lowPoints i

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 i@(_, matrix) = product $ take 3 $ reverse $ sort $ length . toBasis <$> lowPoints i
  where
  toBasis p = walkGraph matrix [p] (adjacents matrix p)

walkGraph :: Matrix -> [Point] -> [Point] -> [Point]
walkGraph _ acc [] = filter ((/= 9) . snd) acc
walkGraph matrix acc (p@(_, v):points)
  | connected = walkGraph matrix nextAcc (points ++ nextPoints)
  | otherwise = walkGraph matrix acc points
  where
    nextAcc = if p `notElem` acc then p:acc else acc
    nextPoints = [v | v <- adjacents matrix p, v `notElem` nextAcc]
    connected = null [() | (_, vv) <- nextPoints, vv < v]

parseInput :: String -> Input
parseInput s = (values, matrix)
  where
  values = concat $ (\(y, t) ->  (\(x, v) -> ((x, y), v)) <$> t ) <$> zip [0..] (zip [0..] <$> matrix)
  matrix = parseRow <$> lines s
  parseRow l = (\c -> read [c]) <$> l

lowPoints :: Input -> [Point]
lowPoints (values, matrix) = fst <$> filter isLowest ((\p -> (p, adjacents matrix p)) <$> values)
  where
  isLowest ((_, v), adj) = all (> v) $ snd <$> adj

adjacents :: Matrix -> Point -> [Point]
adjacents matrix ((x, y), _) = concat [up, down, left, right]
  where
  up
    | y == 0 = []
    | otherwise = getPoint matrix <$> [(x, y - 1)]
  down
    | y == maxY matrix = []
    | otherwise = getPoint matrix <$> [(x, y + 1)]
  left
    | x == 0 = []
    | otherwise = getPoint matrix <$> [(x - 1, y)]
  right
    | x == maxX matrix = []
    | otherwise = getPoint matrix <$> [(x + 1, y)]

maxX :: Matrix -> Int
maxX m = length (head m) - 1

maxY :: Matrix -> Int
maxY m = length m - 1

getPoint :: Matrix -> Coords -> Point
getPoint m c@(x, y) = (c, m !! y !! x)
