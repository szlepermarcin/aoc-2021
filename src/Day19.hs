{-# LANGUAGE ViewPatterns #-}

module Day19
  ( solution
  , part1
  , part2
  )
where

import qualified Data.Set as S
import           Utils

type Coords = (Int, Int, Int)
type Scan = ([Coords], [Coords])
type Hash = Int

type Input = [Scan]

solution :: Solution
solution = (part1, part2, "day19")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 = length . snd . merge
  where
  merge [] = error "empty input"
  merge [x] = x
  merge (x1:xs) = merge (rest ++ [merged])
    where
    (merged, rest) = m x1 [] xs

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 i = maximum dist
  where
  (scanners, _) = merge i
  dist = [  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) | ((x1, y1, z1):ss) <- tails scanners, (x2, y2, z2) <- ss]
  merge [] = error "empty input"
  merge [x] = x
  merge (x1:xs) = merge (rest ++ [merged])
    where
    (merged, rest) = m x1 [] xs


m :: Scan -> [Scan] -> [Scan] -> (Scan, [Scan])
m s acc [] = (s, acc)
m x1@(scanners1, beacons1) acc (x2@(scanners2, beacons2):xs) = case match of
    common | length common < 12 -> next
    common -> fromMaybe next mf
      where
      toTD t = (t, nub $ (\((x1, y1, z1), (x2, y2, z2)) -> (x1 - x2, y1 - y2, z1 - z2)) <$> fmap (second (applyTransformation t)) common)
      mf = do
        (trans, deltas) <- find (\(_, d) -> length d == 1) $ toTD <$> transformations
        let scanners2' = applyDeltas (head deltas) . applyTransformation trans <$> scanners2
        let beacons2' = applyDeltas (head deltas) . applyTransformation trans <$> beacons2
        let merged = (scanners1 ++ scanners2', nub $ beacons1 ++ beacons2')
        return $ m merged acc xs
    where
    match = [ res | h1 <- hashBeacons beacons1, h2 <- hashBeacons beacons2, res <- maybeToList $ checkHashes h1 h2]
    next = m x1 (acc ++ [x2]) xs


parseInput :: String -> Input
parseInput s = res
  where
  res = parseScan <$> splitOn [] (lines s)
  pairs = [ c | ((_,x):ys) <- tails res, (_,y) <- ys, h1 <- hashBeacons x, h2 <- hashBeacons y, c <- maybeToList $ h1 `checkHashes` h2]
  parseScan (x:xs) = ([(0,0,0)], parseLine <$> xs)
  parseScan []     = ([(0,0,0)], [])
  parseLine (splitOn ',' -> [x, y, z]) = (read x, read y, read z)
  parseLine x                          = error $ "invalid scan line: " ++ x


hashBeacons :: [Coords] -> [(Coords, S.Set Hash)]
hashBeacons s = process [] s
  where
  process acc [] = acc
  process acc (v@(x, y, z) : vs)
    | length acc == length s = acc
    | otherwise = process ((v, S.fromList $ (\(x1, y1, z1) -> ((x - x1) ^ 2) +  ((y - y1) ^ 2) + ((z - z1) ^ 2)) <$> vs ) : acc )  (vs ++ [v])

transformations :: [[(Int, Bool)]]
transformations = [[(x, bx), (y, by), (z, bz)] | [x,y,z] <- permutations [0,1,2], bx <- [True, False], by <- [True, False], bz <- [True, False]]

applyDeltas:: (Int, Int, Int) -> Coords -> Coords
applyDeltas (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

applyTransformation :: [(Int, Bool)] -> Coords -> Coords
applyTransformation [(dx, bx), (dy, by), (dz, bz)] (x, y, z) = (nx, ny, nz)
  where
  curr = [x,y,z]
  nx = neg bx (curr !! dx)
  ny = neg by (curr !! dy)
  nz = neg bz (curr !! dz)
  neg b = if b then negate else id
applyTransformation _ _ = error "invalid transformation"

relatives :: Scan -> ([Coords], [(Coords, S.Set Coords)])
relatives (c ,s) = (c, process [] s)
  where
  process acc [] = acc
  process acc (v@(x, y, z) : vs)
    | length acc == length s = acc
    | otherwise = process ((v, S.fromList $ (\(x1, y1, z1) -> (abs $ x - x1, abs $ y - y1, abs $  z - z1)) <$> vs ) : acc )  (vs ++ [v])

checkHashes :: (Coords, S.Set Hash) -> (Coords, S.Set Hash) -> Maybe (Coords, Coords)
checkHashes (c1, h1) (c2, h2) =  if S.size (S.intersection h1 h2) >= 11 then Just (c1, c2) else Nothing
