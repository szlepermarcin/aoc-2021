module Utils
  ( module Utils
  , module Control.Applicative
  , module Control.Monad
  , module Data.Bifunctor
  , module Data.Char
  , module Data.Hashable
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module GHC.Generics
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics

import qualified Data.HashMap.Strict  as Map
import qualified Data.HashSet         as Set
import qualified Data.PQueue.Prio.Min as PQ



chunked :: Int -> [a] -> [[a]]
chunked size xs = chunked' [] [] xs
  where
  chunked' acc curr [] = acc ++ [curr]
  chunked' acc curr (x:xs)
    | length curr == size = chunked' (acc ++ [curr]) [x] xs
    | otherwise = chunked' acc (curr ++ [x]) xs

-- A* implementation from https://gist.github.com/abhin4v/8172534
astarSearch :: (Eq a, Hashable a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> (a -> Int) -> Maybe (Int, [a])
astarSearch startNode isGoalNode nextNodeFn heuristic =
  astar (PQ.singleton (heuristic startNode) (startNode, 0))
         Set.empty (Map.singleton startNode 0) Map.empty
  where
    astar pq seen gscore tracks
      | PQ.null pq           = Nothing
      | isGoalNode node      = Just (gcost, findPath tracks node)
      | Set.member node seen = astar pq' seen gscore tracks
      | otherwise            = astar pq'' seen' gscore' tracks'
      where
        (node, gcost) = snd . PQ.findMin $ pq
        pq'           = PQ.deleteMin pq
        seen'         = Set.insert node seen
        successors    =
          filter (\(s, g, _) -> not (Set.member s seen') &&
                    (not (s `Map.member` gscore) || g < (fromJust . Map.lookup s $ gscore)))
          $ successorsAndCosts node gcost
        pq''    = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors
    successorsAndCosts node gcost = map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node
    findPath tracks node          = if Map.member node tracks
      then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
      else [node]

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
