module Lib
    ( day1
    , part1
    , part2
    ) where

day1 :: IO ()
day1 = do
  putStrLn divider
  putStrLn "day1"
  interact $ \s -> unlines [divider, "part1: " ++ part1 s, divider, "part2: " ++ part2 s]
  putStrLn divider
  return ()
  where
    divider = fmap (const '#') [0..50]


part1 :: String -> String
part1 = (show . processPart1) . map read . lines

processPart1 :: [Int] -> Int
processPart1 a = length $ filter (uncurry (>)) $ zip (tail a) a

part2 :: String -> String
part2 = (show . processPart2) . map read . lines

processPart2 :: [Int] -> Int
processPart2 a = length $ filter (uncurry (>)) $ zip (tail windowSum) windowSum
  where
    b = tail a
    c = tail b
    windowSum = map (\(x, y, z) -> x + y + z) $ zip3 a b c
