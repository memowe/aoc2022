module AoC.Day05.Solution where

import            Data.Char
import            Data.List
import            Data.Maybe
import qualified  Data.Map as M
import            Data.Map ( Map, (!) )
import            Text.ParserCombinators.ReadP

type    Stack a     = [a]
newtype Crates      = Crates (Map Int (Stack Char))   deriving Show
data    Input       = Input Crates [Instruction]      deriving Show
data    Instruction = Instr Int Int Int               deriving Show
data    Move        = Move Int Int                    deriving Show

instance Read Input where
  readsPrec _ = readP_to_S $ do
                  cs <- crates
                  char '\n'
                  is <- many instr
                  return $ Input cs is
    where crates    = do  stacks  <- many stackLine
                          nums    <- sepBy stackNum (char ' ')
                          char '\n'
                          return $ cratesFromLines stacks nums
          stackLine = do  sls <- sepBy (crate +++ noCrate) (char ' ')
                          char '\n'
                          return sls
          crate     = do  char '['
                          name <- get
                          char ']'
                          return $ Just name
          noCrate   = string "   " >> return Nothing
          stackNum  = do  char ' '
                          digits <- satisfy isDigit
                          char ' '
                          return $ read [digits]
          instr     = do  c <- string "move "   >> munch1 isDigit
                          f <- string " from "  >> munch1 isDigit
                          t <- string " to "    >> munch1 isDigit
                          char '\n'
                          return $ Instr (read c) (read f) (read t)
          cratesFromLines cs ns =
            let mCols = transpose cs
                cCols = map (map fromJust . dropWhile isNothing) mCols
            in  Crates $ M.fromList (zip ns cCols)

expandInstrs :: Instruction -> [Move]
expandInstrs (Instr c f t) = replicate c (Move f t)

execute :: Move -> Crates -> Crates
execute (Move f t) (Crates cMap) =
  let thing   = head (cMap ! f)
      remove  = M.adjust tail f
      add     = M.adjust (thing:) t
  in  Crates $ add . remove $ cMap

handleCrates :: Input -> Crates
handleCrates (Input crates instrs) = foldl (flip execute) crates moves
  where moves = concatMap expandInstrs instrs

finalTop :: Crates -> String
finalTop (Crates cMap) = map head (M.elems cMap)

solve05_1 :: String -> String
solve05_1 = finalTop . handleCrates . read

executeInstr :: Instruction -> Crates -> Crates
executeInstr (Instr c f t) (Crates cMap) =
  let things  = take c (cMap ! f)
      remove  = M.adjust (drop c) f
      add     = M.adjust (things ++) t
  in  Crates $ add . remove $ cMap

handleCrates' :: Input -> Crates
handleCrates' (Input crates instrs) = foldl (flip executeInstr) crates instrs

solve05_2 :: String -> String
solve05_2 = finalTop . handleCrates' . read
