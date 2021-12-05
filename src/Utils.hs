module Utils where
import           Data.Char (digitToInt)
import           Data.List (foldl')

type PartSolution = String -> String
type Solution = (PartSolution, PartSolution, String)

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

runner :: String -> PartSolution -> PartSolution -> IO ()
runner day p1 p2 = do
  input <- readFile $ day ++ ".txt"
  putStrLn $ unlines [divider, day, divider, "part1: " ++ p1 input, "part2: " ++ p2 input, divider]
  return ()
  where
    divider = fmap (const '#') [0 .. 50]
