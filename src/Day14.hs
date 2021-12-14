{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Day14
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Pair = String
type Transition = Pair -> [Pair]
type State = [(Pair, Int)]
type Input = (State, [Transition])

solution :: Solution
solution = (part1, part2, "day14")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 = stateValue 10

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 = stateValue 40

parseInput :: String -> Input
parseInput (lines -> s:"":f) = (state, parseTransition <$> f)
  where
  state =  merge $ concat $ (\(c1, c2) -> [([c1, c2],1)]) <$> (s `zip` tail s)
  parseTransition (splitOn ' ' -> [ab@[a,b], "->", [c]]) = \v -> if v == ab then [[a, c], [c, b]] else [] 
  parseTransition _ = const []
parseInput _ = undefined

applyTransitions :: State -> [Transition] -> State
applyTransitions s t = merge $ concat $ (\(p, i) -> concat $ (\tt -> (,i) <$> tt p) <$> t) <$> s

merge :: State -> State
merge s = (\k -> (k, sum $ snd <$> filter (\(kk,_) -> kk == k) s)) <$> nub (fst <$> s)

stateValue :: Int -> Input -> Int
stateValue i (s,t) = maxlv - minlv
  where
  lv = letterVals $ foldl (\ss _ -> applyTransitions ss t) s [1..i]
  maxlv = maximum $ snd <$> lv
  minlv = minimum $ snd <$> lv

letterVals :: State -> State
letterVals s = (\(l, s) -> (l, (s `div` 2) + (s `mod` 2))) <$> merge [([l],v) | (p, v) <- s, l <- p]
