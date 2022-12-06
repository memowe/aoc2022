module AoC.Day01.Solution where

import Data.List
import Data.List.Extra
import Data.Ord

splitElves :: String -> [[Int]]
splitElves = map (map read) . filter (/= [""]) . groupOn null . lines

topElf :: [[Int]] -> Int
topElf = maximum . map sum

topElves :: Int -> [[Int]] -> [Int]
topElves n = take n . sortOn Down . map sum

solve01_1 :: String -> Int
solve01_1 = topElf . splitElves

solve01_2 :: String -> Int
solve01_2 = sum . topElves 3 . splitElves
