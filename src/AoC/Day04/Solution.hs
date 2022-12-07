module AoC.Day04.Solution where

import Data.List
import qualified Data.List.Split as S
import Data.Function
import Text.ParserCombinators.ReadP

data Range a  = Range a a deriving Eq
type Sections = Range Int

sep = '-' :: Char

instance Show a => Show (Range a) where
  show (Range s e) = show s ++ [sep] ++ show e

instance Read a => Read (Range a) where
  readsPrec _ = readP_to_S $ do
                  (start:end:_) <- sepBy (munch1 (/= sep)) (char sep)
                  return $ Range (read start) (read end)

range :: Enum a => Range a -> [a]
range (Range start end) = enumFromTo start end

contains :: (Eq a, Enum a) => Range a -> Range a -> Bool
contains = flip isInfixOf `on` range

containing :: String -> Bool
containing s =  let (r1:r2:_) = map read $ S.wordsBy (== ',') s :: [Sections]
                in  (r1 `contains` r2) || (r2 `contains` r1)

solve04_1 :: String -> Int
solve04_1 = length . filter containing . lines

overlaps :: (Eq a, Enum a) => Range a -> Range a -> Bool
overlaps = (.) (not.null) . (intersect `on` range)

overlapping :: String -> Bool
overlapping s = let (r1:r2:_) = map read $ S.wordsBy (== ',') s :: [Sections]
                in  r1 `overlaps` r2

solve04_2 :: String -> Int
solve04_2 = length . filter overlapping . lines
