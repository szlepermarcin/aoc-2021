module Main where

import qualified Day1  as D1
import qualified Day2  as D2
import qualified Day3  as D3
import           Utils (runner)

solutions =
  [ D1.solution
  , D2.solution
  , D3.solution
  ]

main :: IO ()
main = run solutions
  where
    run [] = return ()
    run ((p1, p2, day) : xs) = runner day p1 p2 >> run xs
