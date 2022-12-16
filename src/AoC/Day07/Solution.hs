module AoC.Day07.Solution where

import Data.Char
import Data.List
import Data.Tree
import Data.Maybe
import Data.Foldable
import Text.ParserCombinators.ReadP

data FSEntry    = Dir String | File Int String
data Input      = Cd String | Ls [FSEntry]
newtype Inputs  = Inputs [Input]

name :: FSEntry -> String
name (Dir n) = n; name (File _ n) = n

instance Read Inputs where
  readsPrec _ = readP_to_S $
    Inputs <$> many (cd +++ ls)
    where cd    = Cd    <$> (string "$ cd"    >> tokLn)
          ls    = Ls    <$> (string "$ ls\n"  >> many (dir +++ file))
          dir   = Dir   <$> (string "dir"     >> tokLn)
          file  = File  <$> fmap read (munch1 isDigit) <*> tokLn
          tokLn = do {char ' '; str <- munch1 (/= '\n'); char '\n'; return str}

newtype Path = Path [String]
  deriving (Eq, Ord, Monoid)

instance Semigroup Path where
  Path ps1 <> Path ps2 = fromMaybe nope $ normalize candidate
    where candidate           = Path $ ps1 ++ ps2
          nope                = error $ "Illegal path: " ++ show candidate
          normalize (Path ps) = Path . reverse <$> collapse (reverse ps)
          collapse []         = Just []
          collapse ("..":ps)  = tail' =<< collapse ps
          collapse (p   :ps)  = (p:)  <$> collapse ps
          tail' xs            = if null xs then Nothing else Just (tail xs)

instance Show Path where
  show (Path parts) = '/' : intercalate "/" parts

instance Read Path where
  readsPrec _ = readP_to_S $
    Path <$> (optional (char '/') >> munch1 (/= '/') `sepBy` char '/')

inside :: Path -> Path -> Bool
Path desc `inside` Path parent = parent `isPrefixOf` desc

type Listing    = [(Path, FSEntry)]
type SizedPath  = (Path, Int)
type FileSystem = Tree SizedPath

readInputs :: Inputs -> Listing
readInputs (Inputs is) = sortOn fst . snd $ foldl consume (mempty, []) is
  where consume (p, xs) (Cd p') = (p <> read p', xs)
        consume (p, xs) (Ls es) = (p, xs ++ map ((,) =<< prepend p) es)
          where p `prepend` entry = p <> read (name entry)

buildFilesystem :: Listing -> FileSystem
buildFilesystem lng = let fss = build lng
                      in  Node (mempty, allSize fss) fss
  where allSize = sum . fmap (snd . rootLabel)
        build [] = []
        build ((path, f@(File size _))  : es) = Node (path, size) [] : build es
        build ((path, d@(Dir _))        : es) =
          let (i,o)       = span ((`inside` path) . fst) es
              (ins, outs) = (build i, build o)
              inSize      = allSize ins
          in  Node (path, inSize) ins : outs

drawFilesystem :: FileSystem -> String
drawFilesystem = drawTree . fmap show

dirs :: FileSystem -> [SizedPath]
dirs (Node _ [])  = []
dirs (Node sp cs) = sp : concatMap dirs cs

solve07_1 :: String -> Int
solve07_1 = sum . filter (<= 100000) . map snd . dirs
          . buildFilesystem . readInputs . read

solve07_2 :: String -> Int
solve07_2 input = fromJust $ find (>= minDel) (sort sizes)
  where sizes   = map snd . dirs . buildFilesystem . readInputs . read $ input
        free    = 70000000 - head sizes
        minDel  = 30000000 - free
