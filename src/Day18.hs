{-# LANGUAGE ViewPatterns #-}
module Day18
  ( solution
  , part1
  , part2
  )
where

import           Utils

data Token = O | C | V Int

type Exp = [Token]
type Input = [Exp]

solution :: Solution
solution = (part1, part2, "day18")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 (x:xs) = fromJust . magnitude $ foldl add x xs
processPart1 _     = error "invalid input"

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 i = maximum  $ fromJust . magnitude <$> [s | (prev, x : next) <- zip (inits i) (tails i), y <- prev ++ next, s <- [add x y, add y x]]

parseInput :: String -> Input
parseInput s = tokenize <$> lines s

tokenize :: String -> [Token]
tokenize = tokenize' [] ""
  where
  tokenize' acc _ [] = acc
  tokenize' acc currLit ('[':xs) = tokenize' (acc ++ [O]) currLit xs
  tokenize' acc currLit (',':xs)
    | not $ null currLit = tokenize' (acc ++ [V $ read currLit]) "" xs
    | otherwise = tokenize' acc currLit xs
  tokenize' acc currLit (']':xs)
    | not $ null currLit = tokenize' (acc ++ [V $ read currLit] ++ [C]) "" xs
    | otherwise = tokenize' (acc ++ [C]) currLit xs
  tokenize' acc currLit (x:xs) = tokenize' acc (currLit ++ [x]) xs

add :: Exp -> Exp -> Exp
add e1 e2 = explode 0 [] (O : e1 ++ e2 ++ [C])
  where
  explode n prev (O : V v1 : V v2 : C : next)
    | n >= 4 = explode 0 [] $ reverse (mfv (+ v1) prev) ++ V 0 : mfv (+ v2) next
  explode n prev (cur : next) = explode n' (cur : prev) next
    where
    n' 
      | O <- cur = n + 1
      | C <- cur = n - 1
      | otherwise = n
  explode _ prev [] = split [] $ reverse prev
  mfv f (V x : rest) = V (f x) : rest
  mfv f (x : rest)   = x : mfv f rest
  mfv _ []           = []
  split prev (V x : next) 
    | x > 9 = explode 0 [] $ reverse prev ++ O : V (x `div` 2) : V ((x + 1) `div` 2) : C : next
  split prev (cur : next) = split (cur : prev) next
  split k [] = reverse k

magnitude :: Exp -> Maybe Int
magnitude exp
  | (result, []) <- magnitude' exp = result
  | otherwise = Nothing
  where
  magnitude' (O : (magnitude' -> (Just lhs, magnitude' -> (Just rhs, C : rest)))) = (Just $ 3 * lhs + 2 * rhs, rest)
  magnitude' (V value : rest) = (Just value, rest)
  magnitude' rest = (Nothing, rest)
