module AoC.Day02.Solution where

score :: Char -> Char -> Int
score 'A' 'X' = 1 + 3
score 'A' 'Y' = 2 + 6
score 'A' 'Z' = 3 + 0
score 'B' 'X' = 1 + 0
score 'B' 'Y' = 2 + 3
score 'B' 'Z' = 3 + 6
score 'C' 'X' = 1 + 6
score 'C' 'Y' = 2 + 0
score 'C' 'Z' = 3 + 3

lineScore :: String -> Int
lineScore (l:' ':r:_) = score l r
lineScore s           = error ("Illegal input: " ++ s)

solve02_1 :: String -> Int
solve02_1 = sum . map lineScore . lines

solve02_2 :: String -> Int
solve02_2 = undefined
