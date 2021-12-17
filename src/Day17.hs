{-# LANGUAGE ViewPatterns #-}
module Day17
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Range = (Int, Int)
type Input = (Range, Range)

solution :: Solution
solution = (part1, part2, "day17")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 i = sum [0..(maximum (snd <$> xy i))] 

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 i = length $ xy i

parseInput :: String -> Input
parseInput (splitOn ' ' -> [_, _, rawx, rawy] ) = (parseRange rawx, parseRange rawy)
  where
  parseRange (parseRange' -> [v1, v2]) = (read v1, read v2)
  parseRange x = error $ "invalid input" ++ show x
  parseRange' x = filter (not . null) (splitOn '.' (filter (`notElem` "xy,=") x))
parseInput x = error $ "invalid input" ++ x

xy :: Input -> [(Int, Int)]
xy ((xmin, xmax), (ymin, ymax)) = nub [(x,y) | (y,sy) <- ys, (x, sx) <- xs, sy == sx ]
  where
  xrange = [xmin..xmax]
  yrange = [ymin..ymax]
  ys = 
    [ (y, i) 
    | y <- [ymin..(abs ymin - 1)]
    , (i,s) <- zip [0..] (takeWhile (>= ymin) (scanl (+) 0 (iterate (+ (-1)) y)))
    , s `elem` yrange
    ]
  xs = 
    [ (x, i) 
    | x <- [0..xmax], sum [0..x] > xmin
    , (i,s) <- zip (takeWhile (<= maximum (snd <$> ys) ) [0..]) (takeWhile (<= xmax) (scanl (+) 0 ([x, x-1..0] ++ repeat 0 )))
    , s `elem` xrange 
    ]
