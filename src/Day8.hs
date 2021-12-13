module Day8
  ( solution
  , part1
  , part2
  )
where

import           Utils

type Digit = String
type Mapping = String

solution :: Solution
solution = (part1, part2, "day8")

part1 :: String -> String
part1 = show . processPart1 . parseInput

processPart1 :: [([Digit], [Digit])] -> Int
processPart1 i = sum $ countUnique <$> i
  where
  countUnique (_, digits) = length $ filter (\d -> or $ (length d ==) <$> [2, 3, 4, 7]) digits

part2 :: String -> String
part2 = show . processPart2 . parseInput

processPart2 :: [([Digit], [Digit])] -> Int
processPart2 i = sum $ extractNumber . fixDigits <$> i
  where
  extractNumber (_, d) = sum $ zipWith (*) (decodeDigit <$> reverse d) (iterate (10*) 1)

parseInput :: String -> [([Digit], [Digit])]
parseInput s = parseLine <$> lines s
  where
  parseLine ss = case splitOn '|' ss of
    [p1, p2] -> (parsePart p1, parsePart p2)
    _        -> undefined
  parsePart = filter (not . null) . splitOn ' '

digitPatterns :: [Digit]
digitPatterns =
  [ "abcefg"
  , "cf"
  , "acdeg"
  , "acdfg"
  , "bcdf"
  , "abdfg"
  , "abdefg"
  , "acf"
  , "abcdefg"
  , "abcdfg"
  ]

decodeDigit :: Digit -> Int
decodeDigit d = fromMaybe (-1) (elemIndex (sort d) digitPatterns)

fixDigits :: ([Digit], [Digit]) -> ([Digit], [Digit])
fixDigits (d1, d2) = (mapDigit mapping <$> d1, mapDigit mapping <$> d2)
  where
  mapping = extractMapping (d1 ++ d2)

extractMapping :: [Digit] -> Mapping
extractMapping digits = head $ filter isValid $ permutations "abcdefg"
  where
  isValid mapping = not (any (\d -> decodeDigit d == -1 ) (mapDigit mapping <$> digits))

mapDigit :: Mapping -> Digit -> Digit
mapDigit m d = sort $ convert m <$> d
  where
  convert m c = "abcdefg" !! fromJust (elemIndex c m)
