{-# LANGUAGE ViewPatterns #-}
module Day15
  ( solution
  , part1
  , part2
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Utils


type Coords   = (Int, Int)
type Input = M.Map Coords Int
type Memo  = M.Map Coords Int
type Queue  = S.Set Coords

solution :: Solution
solution = (part1, part2, "day15")

part1 :: String -> String
part1 = show . process . parseInput

part2 :: String -> String
part2 = show . process . multiply 5 . parseInput

parseInput :: String -> Input
parseInput s = M.fromList l
  where
  l = [((x, y), read [v]) | (y, line) <- zip [0..] (lines s), (x, v) <- zip [0..] line]
  maxx = maximum $ fst . fst <$> l
  maxy = maximum $ snd . fst <$> l

process :: Input -> Int
process i = process' (M.singleton (0, 0) 0) (S.singleton (0, 0))
  where
    end@(ex, ey) = maximum $ M.keys i
    process' :: Memo -> Queue -> Int
    process' memo (S.minView -> Nothing)                 = memo M.! end
    process' memo (S.minView -> Just (pos@(x, y), rest)) = process' memo' queue'
      where
        queue' = foldr (S.insert . fst) rest pr
        memo' = foldr (uncurry M.insert) memo pr
        pr =
          [ (p, r)
          | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
          , p@(nx, ny) <- [(x + dx, y + dy)]
          , 0 <= nx && nx <= ex && 0 <= ny && ny <= ey
          , r <- [(i M.! p) + get pos]
          , get p > r
          ]
        get = fromMaybe maxBound . (M.!?) memo

multiply :: Int -> Input -> Input
multiply n i = M.fromList
    [ ((x + mx * sx, y + my * sy), (mx + my + v - 1) `mod` 9 + 1)
    | ((x, y), v) <- M.toList i
    , mx <- [0..n - 1]
    , my <- [0..n - 1]
    ]
  where
    (maxx, maxy) = maximum $ M.keys i
    (sx, sy) = (maxx + 1, maxy + 1)
