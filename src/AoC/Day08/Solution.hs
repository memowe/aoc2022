module AoC.Day08.Solution where

import Data.Char
import Data.Bifunctor
import qualified Data.Map as M

type Row        = Int
type Col        = Int
type TreeHeight = Int
type Grid       = M.Map (Col, Row) TreeHeight

readGrid :: String -> Grid
readGrid = M.fromList . concatMap injectRow . linesWithRows . lines
  where injectRow     = uncurry $ map . first . flip (,)
        linesWithRows = zip [0..] . map readLine
        readLine      = zip [0..] . map digitToInt

solve08_1 :: String -> Int
solve08_1 = undefined

solve08_2 :: String -> Int
solve08_2 = undefined
