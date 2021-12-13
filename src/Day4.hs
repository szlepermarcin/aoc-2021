module Day4
  ( solution
  , part1
  , part2
  )
where

import           Utils

solution :: Solution
solution = (part1, part2, "day4")

type Game = [Int]
type Board = [[(Int, Bool)]]


isWinning :: Board -> Bool
isWinning b = hasWinningRow b || hasWinningColumn b
    where
    hasWinningRow = or . fmap (and . fmap snd)
    hasWinningColumn = hasWinningRow . transpose

markNumber :: Int -> Board -> Board
markNumber n = fmap (fmap mark)
  where
  mark (curr, v)
    | curr == n = (curr, True)
    | otherwise = (curr, v)

boardValue :: Int -> Board -> Int
boardValue m b = m * getSum (boardValue' 0 b)
    where
    boardValue' acc [] = acc
    boardValue' acc (x:xs) = boardValue' (acc + foldMap (Sum . fst) (filter (not . snd) x)) xs


parseData :: String -> (Game, [Board])
parseData = parse . lines
    where
    parse (x:xs) = (parseGame x, parseBoards xs)
    parse _ = ([], [])
    parseGame x = fmap read (splitOn ',' x)
    parseBoards :: [String] -> [Board]
    parseBoards xs = filter (not . null) (fmap parseBoard (splitOn [] xs))
    parseBoard :: [String] -> Board
    parseBoard l = fmap parseBoardRow l
    parseBoardRow :: String -> [(Int, Bool)]
    parseBoardRow r = zip (fmap read (filter (not . null) (splitOn ' ' r))) (repeat False)


part1 :: String -> String
part1 s = show $ processPart1 game boards
    where
    (game, boards) = parseData s


processPart1 :: Game -> [Board] -> Int
processPart1 [] _ = -1
processPart1 (x:xs) b = if any isWinning processed then boardValue x winning else processPart1 xs processed
    where
    processed = fmap (markNumber x) b
    winning = head $ filter isWinning processed

part2 :: String -> String
part2 s = show $ processPart2 0 game boards
    where
    (game, boards) = parseData s

processPart2 :: Int -> Game -> [Board] -> Int
processPart2 acc [] _ = acc
processPart2 acc (x:xs) b = if any isWinning processed then processPart2 (boardValue x winning) xs filtered else processPart2 acc xs filtered
    where
    processed = fmap (markNumber x) b
    winning = head $ filter isWinning processed
    filtered = filter (not . isWinning) processed
