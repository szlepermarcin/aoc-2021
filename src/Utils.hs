module Utils
  ( module Utils
  , module Data.Bifunctor
  , module Data.Char
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Monoid


type PartSolution = String -> String
type Solution = (PartSolution, PartSolution, String)

runner :: String -> PartSolution -> PartSolution -> IO ()
runner day p1 p2 = do
  input <- readFile $ "inputs/" ++ day ++ ".txt"
  putStrLn $ unlines [divider, day, divider, "part1: " ++ p1 input, "part2: " ++ p2 input, divider]
  return ()
  where
    divider = fmap (const '#') [0 .. 50]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: Eq a => [[a]] -> a -> [a] -> [[a]]
splitOn' acc _ [] = acc
splitOn' acc d x = case span (/= d) x of
  (v, vs) -> case vs of
    (_:vvs) -> splitOn' (acc ++ [v]) d vvs
    []      -> acc ++ [v]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

set :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
set i c v = (\(cc, vv) -> if cc == c then (cc, v) else (cc, vv)) <$> i

modify :: Eq a => [(a, b)] -> a -> (b -> b) -> [(a, b)]
modify i c f = (\(cc, vv) -> if cc == c then (cc, f vv) else (cc, vv)) <$> i
