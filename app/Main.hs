module Main where

import qualified Day1  as D1
import qualified Day2  as D2
import qualified Day3  as D3
import qualified Day4  as D4
import qualified Day5  as D5
import qualified Day6  as D6
import qualified Day7  as D7
import           Utils (runner)

solutions =
  [ D1.solution
  , D2.solution
  , D3.solution
  , D4.solution
  , D5.solution
  , D6.solution
  , D7.solution
  ]

main :: IO ()
main = run solutions
  where
    run [] = return ()
    run ((p1, p2, day) : xs) = runner day p1 p2 >> run xs
