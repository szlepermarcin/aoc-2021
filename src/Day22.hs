{-# LANGUAGE ViewPatterns #-}

module Day22
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Coord = (Int, Int, Int)
type Cuboid = (Coord, Coord)

type Input = [(Bool, Cuboid)]

solution :: Solution
solution = (part1, part2, "day22")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 i = process True small
  where
  small = filter (\(_,((x1, y1, z1), (x2, y2, z2))) -> all (\v -> v >= -50 && v <= 50) [x1, x2, y1, y2, z1, z2]) i

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 = process True

parseInput :: String -> Input
parseInput s = parseLine . splitOn ' ' <$> lines s
  where
  parseLine ["on", rest]  = (True, parseCoords $ splitOn ',' rest)
  parseLine ["off", rest] = (False, parseCoords $ splitOn ',' rest)
  parseLine _             = error "invalid input"
  parseCoords [x,y,z] = ((minx, miny, minz), (maxx, maxy, maxz))
    where
    [minx, maxx] = read <$> filter (not . null) (splitOn '.' $ drop 2 x)
    [miny, maxy] = read <$> filter (not . null) (splitOn '.' $ drop 2 y)
    [minz, maxz] = read <$> filter (not . null) (splitOn '.' $ drop 2 z)
  parseCoords _ = error "invalid input"

process :: Bool -> Input -> Int
process on (dropWhile ((/= on) . fst) -> (_, c@((x1, y1, z1), (x2, y2, z2))):rest ) =
  size c
    - process (not on) (mapMaybe (transform (inner (x1, x2)) (inner (y1, y2)) (inner (z1, z2))) rest)
    + process on (mapMaybe (transform Just Just (lower z1)) rest)
    + process on (mapMaybe (transform Just Just (greater z2)) rest)
    + process on (mapMaybe (transform Just (lower y1) (inner (z1, z2))) rest)
    + process on (mapMaybe (transform Just (greater y2) (inner (z1, z2))) rest)
    + process on (mapMaybe (transform (lower x1) (inner (y1, y2)) (inner (z1, z2))) rest)
    + process on (mapMaybe (transform (greater x2) (inner (y1, y2)) (inner (z1, z2))) rest)
  where
  transform tx ty tz (b, ((x1, y1, z1), (x2, y2, z2))) = do
      (nx1, nx2) <- tx (x1, x2)
      (ny1, ny2) <- ty (y1, y2)
      (nz1, nz2) <- tz (z1, z2)
      return (b, ((nx1, ny1, nz1), (nx2, ny2, nz2)))
  size ((x1, y1, z1), (x2, y2, z2)) = ((x2 - x1) + 1) * ((y2 - y1) + 1) * ((z2 - z1) + 1)
  greater maxv (v1, v2) = guard (v2 > maxv) >> pure ( max (maxv + 1) v1, v2)
  lower minv (v1, v2) = guard (v1 < minv) >> pure (v1, min (minv - 1) v2)
  inner (minv, maxv) (v1, v2) = guard (v1 <= maxv && v2 >= minv) >> pure (max minv v1, min maxv v2)
process _ _ = 0
