{-# LANGUAGE TupleSections #-}
module Day12
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Input = [(String, [String])]

solution :: Solution
solution = (part1, part2, "day12")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 i = length $ walk i valid [] [["start"]]
  where
  valid v a = not (isSmall v && v `elem` a)

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 i = length $ walk i valid [] [["start"]]
  where
  valid v@"start" a = v `notElem` a
  valid v@"end" a = v `notElem` a
  valid v a@(x:_)
    | isSmall v = case length $ filter (== v) a of
      0 -> True
      1 -> maximum (length <$> group ( sort $ filter isSmall a)) < 2
      _ -> False
    | otherwise = True
  valid v [] = True

parseInput :: String -> Input
parseInput s = foldr (\xs acc -> foldr (\(v:vs) acc2 -> modify acc2 v (++ vs)) acc (permutations xs)) initial $ parseLine <$> lines s
  where
  initial = (, []) <$> nub (concat parsed)
  parsed = parseLine <$> lines s
  parseLine :: String -> [String]
  parseLine = splitOn '-'

walk :: Input -> (String -> [String] -> Bool) -> [[String]] -> [[String]] -> [[String]]
walk i _ acc [] = acc
walk i valid acc curr = walk i valid newAcc newCurr
  where
  step :: [[String]]
  step = concat $ (\a@(x:_) ->  (: a) <$> filter (`valid` a) (fromMaybe [] (lookup x i)))  <$> curr
  (finished, newCurr) = partition isFinished step
  isFinished ("end":_) = True
  isFinished _         = False
  newAcc = acc ++ finished

isSmall :: String -> Bool
isSmall (x:_) = isLower x
isSmall _     = False
