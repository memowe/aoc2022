module AoC.Day03.Solution where

import Data.List
import Data.List.Split
import Data.Maybe

shared :: Eq a => [a] -> [a]
shared xs = let half = length xs `div` 2
            in  nub $ intersect (take half xs) (drop half xs)

priority :: Char -> Int
priority = succ . fromJust . (`elemIndex` items)
  where items = ['a'..'z'] ++ ['A'..'Z']

rucksackPrio :: String -> Int
rucksackPrio = sum . map priority . shared

solve03_1 :: String -> Int
solve03_1 = sum . map rucksackPrio . lines

badges :: Eq a => [[a]] -> [a]
badges = nub . foldr1 intersect

badgePrio :: [String] -> Int
badgePrio = sum . map priority . badges

solve03_2 :: String -> Int
solve03_2 = sum . map badgePrio . chunksOf 3 . lines
