module AoC.Day06.Solution where

import Data.List
import Data.Maybe

firstNdifferent :: Eq a => Int -> [a] -> Maybe Int
firstNdifferent n xs
  | length xs < n     = Nothing
  | uniq (take n xs)  = Just n
  | otherwise         = succ <$> firstNdifferent n (tail xs)
  where uniq = (==) <*> nub

solve06_1 :: String -> Int
solve06_1 = fromJust . firstNdifferent 4

solve06_2 :: String -> Int
solve06_2 = fromJust . firstNdifferent 14
