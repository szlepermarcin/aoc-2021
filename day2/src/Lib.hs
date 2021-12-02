module Lib
    ( day2
    , part1
    , part2
    ) where

day2 :: IO ()
day2 = do
  putStrLn divider
  putStrLn "day2"
  interact $ \s -> unlines [divider, "part1: " ++ part1 s, divider, "part2: " ++ part2 s]
  putStrLn divider
  return ()
  where
    divider = fmap (const '#') [0..50]


part1 :: String -> String
part1 = (show . processPart1) . map parseCommand . lines
  where parseCommand line = case splitOn ' ' line of [pos, value] -> (pos, read value)

processPart1 :: [(String, Int)] -> Int
processPart1 = calc 0 0
  where
  calc horiz depth [] = horiz * depth
  calc horiz depth (("forward", v):xs) = calc (horiz + v) depth xs
  calc horiz depth (("down", v):xs) = calc horiz (depth + v) xs
  calc horiz depth (("up", v):xs) = calc horiz (depth - v) xs



part2 :: String -> String
part2 = (show . processPart2) . map parseCommand . lines
  where parseCommand line = case splitOn ' ' line of [pos, value] -> (pos, read value)


processPart2 :: [(String, Int)] -> Int
processPart2 = calc 0 0 0
  where
  calc horiz depth aim [] = horiz * depth
  calc horiz depth aim (("forward", v):xs) = calc (horiz + v) (depth + (v * aim)) aim xs
  calc horiz depth aim (("down", v):xs) = calc horiz depth (aim + v) xs
  calc horiz depth aim (("up", v):xs) = calc horiz depth (aim - v) xs



splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: Eq a => [[a]] -> a -> [a] -> [[a]]
splitOn' acc _ [] = acc
splitOn' acc d x = case span (/= d) x of
  (v, vs) -> case vs of
    (_:vvs) -> splitOn' (acc ++ [v]) d vvs
    []      -> acc ++ [v]
