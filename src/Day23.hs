{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Day23
  ( solution
  , part1
  , part2
  )
where

import qualified Data.HashMap.Strict as M
import           Utils

data Amphipod = A | B | C | D deriving (Show, Eq, Ord, Enum, Generic, Read)

instance Hashable Amphipod

type Input = ([Amphipod], [Amphipod])
type State = M.HashMap (Int, Int) Amphipod

solution :: Solution
solution = (part1, part2, "day23")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 (l1, l2) = {-debug-} result
  where
  finished = [A .. D]
  (result, paths) = process $ mkState $ concat $ zipWith4 (\a b c d -> [a,b,c,d]) l1 l2 finished finished
  -- debug x = (trace $ unlines (["", replicate 20 '#'] ++ (prettyPrint 2 <$> paths) ++ [replicate 20 '#', ""])) x

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 (l1, l4) = {-debug-} result
  where
  l2 = [D, C, B, A]
  l3 = [D, B, A, C]
  (result, paths) = process $ mkState $ concat $ zipWith4 (\a b c d -> [a,b,c,d]) l1 l2 l3 l4
  -- debug x = (trace $ unlines (["", replicate 20 '#'] ++ (prettyPrint 4 <$> paths) ++ [replicate 20 '#', ""])) x

process :: State -> (Int, [State])
process i = result
  where
  result =  fromMaybe (0, []) $ astarSearch i (end ==) possibleStates stateValue

parseInput :: String -> Input
parseInput s = (a1, a2)
  where
  [a1, a2] = parseLine <$> take 2 (drop 2 $ lines s)
  parseLine l = [ read [c] | c <- l, c `elem` "ABCD" ]

energy :: Amphipod -> Int
energy = (10^) . fromEnum

room :: Amphipod -> Int
room = (* 2) . (+ 1) . fromEnum

end :: State
end = mkState $ [A .. D] >>= replicate 4

spaces :: [Int]
spaces = [1 .. 4]

rooms :: [Int]
rooms = [2, 4 .. 8]

hallway :: [Int]
hallway = filter (not . flip elem rooms) [0 .. 10]

mkState :: [Amphipod] -> State
mkState = M.fromList . zip ((,) <$> rooms <*> spaces)

steps :: State -> ((Int, Int), Amphipod) -> [(Int, State)]
steps state ((x, y), a)
  | y == 0 = maybeToList $ guard (all available spaces) >> foldl1 (<|>) ( walk . (,) (room a) <$> reverse spaces)
  | x == room a, all available spaces = []
  | otherwise = mapMaybe walk $ (, 0) <$> hallway
  where
  without = M.delete (x, y) state
  walk (xt, yt) = guard (y == 0 && path without x xt yt || y > 0 && path without xt x y) $> (energy a * (abs (x - xt) + abs (y - yt)), M.insert (xt, yt) a without)
  available n = maybe True (== a) $ M.lookup (room a, n) state

path :: State -> Int -> Int -> Int -> Bool
path state x xt yt = not . any (`M.member` state) $ map (, 0) [min x xt .. max x xt] ++ map (xt,) [1 .. yt]

possibleStates :: State -> [(State, Int)]
possibleStates s =
  [ (vv, neigh)
  | e <- M.toList s
  , (neigh, vv) <- steps s e
  ]

stateValue :: State -> Int
stateValue s = sum [abs (c - room v) * energy v | ((c,_), v) <- M.toList s]

prettyPrint :: Int -> State -> String
prettyPrint depth s = unlines $
  [ replicate 13 '#'
  , "#" ++ (concat [getCell (i,0) | i <- [0..10]]) ++ "#"
  , "###" ++ rooms 1 ++ "###"
  ] ++
  [ "  #" ++ rooms i ++ "#  " | i <- [2..depth]] ++
  ["  #########  "]
  where
  getCell coords = maybe "." show (s M.!? coords)
  rooms level = getCell (2, level) ++ "#" ++ getCell (4, level) ++ "#" ++ getCell (6, level) ++ "#" ++ getCell (8, level)
