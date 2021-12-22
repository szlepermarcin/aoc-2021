module Main where

import qualified Day1  as D1
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12
import qualified Day13 as D13
import qualified Day14 as D14
import qualified Day15 as D15
import qualified Day16 as D16
import qualified Day17 as D17
import qualified Day18 as D18
import qualified Day19 as D19
import qualified Day20 as D20
import qualified Day2  as D2
import qualified Day3  as D3
import qualified Day4  as D4
import qualified Day5  as D5
import qualified Day6  as D6
import qualified Day7  as D7
import qualified Day8  as D8
import qualified Day9  as D9
import           Utils (runner)

solutions =
  [ D1.solution
  , D2.solution
  , D3.solution
  , D4.solution
  , D5.solution
  , D6.solution
  , D7.solution
  , D8.solution
  , D9.solution
  , D10.solution
  , D11.solution
  , D12.solution
  , D13.solution
  , D14.solution
  , D15.solution
  , D16.solution
  , D17.solution
  , D18.solution
  , D19.solution
  , D20.solution
  ]

main :: IO ()
main = run solutions
  where
    run []                   = return ()
    run ((p1, p2, day) : xs) = runner day p1 p2 >> run xs
