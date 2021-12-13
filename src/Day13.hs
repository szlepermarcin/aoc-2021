module Day13
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Coords = (Int, Int)
data Fold = H Int | V Int | Id deriving Show

type Input = ([Coords], [Fold])

solution :: Solution
solution = (part1, part2, "day13")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 (s,f) = length $ foldl (flip toFn) s (take 1 f)

part2 :: String -> String
part2 = processPart2 . parseInput

processPart2 :: Input -> String
processPart2 (s,f) = unlines ("" : rows)
  where
  coords = foldl (flip toFn) s f
  maxX = maximum $ fst <$> coords
  maxY = maximum $ snd <$> coords
  rows = mkRow <$> [0..maxY]
  mkRow y = (`mkChar` y) <$> [0..maxX]
  mkChar x y = if (x,y) `elem` coords then '#' else ' '

parseInput :: String -> Input
parseInput s = (coords, folds)
  where
  [c, f] = splitOn [] $ lines s
  coords = parseCoord <$> c
  parseCoord cc = mkCoord $ splitOn ',' cc
  mkCoord [x,y] = (read x, read y)
  mkCoord _     = undefined
  folds = parseFold <$> f
  parseFold ff = mkFold $ splitOn '=' ff
  mkFold ["fold along y", y] = H (read y)
  mkFold ["fold along x", x] = V (read x)
  mkFold _                   = Id

toFn :: Fold -> [Coords] -> [Coords]
toFn (H i) = fh i
toFn (V i) = fv i
toFn Id    = id

fh :: Int -> [Coords] -> [Coords]
fh i v = adjust $ nub $ translate <$> v
  where
  translate (x, y)
    | y > i = (x, i - (y - i))
    | otherwise = (x, y)

fv :: Int -> [Coords] -> [Coords]
fv i v = adjust $ nub $ translate <$> v
  where
  translate (x, y)
    | x > i = (i - (x - i), y)
    | otherwise = (x, y)

adjust :: [Coords] -> [Coords]
adjust c = adjusted
  where
  adjx = negate (minimum (fst <$> c))
  adjy = negate (minimum (snd <$> c))
  adjusted = bimap (adjx +) (adjy +) <$> c
