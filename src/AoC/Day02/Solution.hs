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

lineScore :: (Char -> Char -> Int) -> String -> Int
lineScore sf  (l:' ':r:_) = sf l r
lineScore _   s           = error ("Illegal input: " ++ s)

solve02_1 :: String -> Int
solve02_1 = sum . map (lineScore score) . lines

score' :: Char -> Char -> Int
score' 'A' 'X' = score 'A' 'Z'
score' 'A' 'Y' = score 'A' 'X'
score' 'A' 'Z' = score 'A' 'Y'
score' 'B' 'X' = score 'B' 'X'
score' 'B' 'Y' = score 'B' 'Y'
score' 'B' 'Z' = score 'B' 'Z'
score' 'C' 'X' = score 'C' 'Y'
score' 'C' 'Y' = score 'C' 'Z'
score' 'C' 'Z' = score 'C' 'X'

solve02_2 :: String -> Int
solve02_2 = sum . map (lineScore score') . lines
