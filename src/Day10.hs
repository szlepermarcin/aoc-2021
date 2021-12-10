module Day10
  ( solution
  , part1
  , part2
  )
where

import           Data.List
import           Data.Maybe
import           Utils

type Input = [String]

solution :: Solution
solution = (part1, part2, "day10")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: Input -> Int
processPart1 l = sum $ getScore1 . snd . processLine [] [] <$> l

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: Input -> Int
processPart2 l = middleVal $ getScore2 . toClosing . fst <$> filter (null . snd) (processLine [] [] <$> l)
  where
  middleVal x = sort x !! (length x `div` 2)
  toClosing x = toClosing' <$> x
  toClosing' '(' = ')'
  toClosing' '[' = ']'
  toClosing' '{' = '}'
  toClosing' '<' = '>'
  toClosing' x = x

parseInput :: String -> Input
parseInput = lines

processLine :: String -> String -> String -> (String, String)
processLine stack err [] = (stack, err)
processLine [] err (x:xs)
  | isClosing x = processLine [] (x:err) xs
  | otherwise = processLine [x] err xs
processLine (s:ss) err (x:xs)
  | isClosing x && isMatching s x = processLine ss err xs
  | isClosing x = processLine (s:ss) (x:err) xs
  | otherwise = processLine (x:s:ss) err xs

isClosing :: Char -> Bool
isClosing c = c `elem` ")]}>"

isMatching :: Char -> Char -> Bool
isMatching c1 c2   = [c1, c2] `elem` ["()", "[]", "{}", "<>"]


getScore1 :: String -> Int
getScore1 []     = 0
getScore1 [')']  = 3
getScore1 [']']  = 57
getScore1 ['}']  = 1197
getScore1 ['>']  = 25137
getScore1 (_:xs) = getScore1 xs

getScore2 :: String -> Int
getScore2 = getScore' 0
  where
  getScore' acc []     = acc
  getScore' acc (c:cs) = getScore' (5 * acc + valueOf c) cs
  valueOf x = fromMaybe (-1) (x `elemIndex` ")]}>") + 1

