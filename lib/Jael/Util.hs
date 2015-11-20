module Jael.Util where

import Data.Char (digitToInt)
import qualified Data.List.NonEmpty as NE
import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import Numeric (readInt, readDec, readHex, readOct)
import Text.Read (ReadS)
import Jael.Grammar

wrongNumberOfElements :: Integer -> String -> String -> a
wrongNumberOfElements n x y = error $ "Expected exactly " ++ show n ++ x ++ "s in " ++ y

notEnoughElements :: Integer -> String -> String -> a
notEnoughElements n x y = error $ "Expected at least " ++ show n ++ x ++ "s in " ++ y

addIfUnique :: Ord a => (a, b) -> M.Map a b -> Maybe (M.Map a b)
addIfUnique (k, v) m = case M.insertLookupWithKey (\_ n _ -> n) k v m of
                            (Nothing, m') -> Just m'
                            (_, _) -> Nothing

collectDup :: Ord a => (a, b) -> ([a], M.Map a b) -> ([a], M.Map a b)
collectDup ins@(k, _) (col, m) = case addIfUnique ins m of
                                      Nothing -> (k:col, m)
                                      Just m' -> (col, m')

-- Given an environment and a list of tuples to add, returns either a list of
-- duplicate names or a new environment with the elements of the list added.
insertCollectDups :: Ord a => M.Map a b -> [(a, b)] -> Either [a] (M.Map a b)
insertCollectDups m xs = let (dups, m') = foldr collectDup ([], m) xs
                   in if null dups
                         then Right m'
                         else Left dups

-- NonEmpty version of https://hackage.haskell.org/package/Unique
repeated :: Ord a => [a] -> [a]
repeated = map NE.head . filterByLength (>1)

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [NE.NonEmpty a]
filterByLength p = filter (p . length) . NE.group . sort

parseIntErrorMsg :: String
parseIntErrorMsg = "Lexer should not produce integer strings that parse\
                   \ incorrectly. See definition in Grammar.cf"

readBin :: ReadS Integer
readBin = readInt 2 (\x -> x == '0' || x == '1') digitToInt

intDoParse :: ReadS Integer -> String -> Integer
intDoParse p xs =
  case p xs of
       [(i, [])] -> i
       _         -> error parseIntErrorMsg

parseAnyInt :: GAnyInt -> Integer
parseAnyInt (GAnyIntDecInt x) = parseDecInt x
parseAnyInt (GAnyIntHexInt x) = parseHexInt x
parseAnyInt (GAnyIntOctInt x) = parseOctInt x
parseAnyInt (GAnyIntBinInt x) = parseBinInt x

parseDecInt :: DecInt -> Integer
parseDecInt (DecInt s@(x:xs)) = if x == '~'
                                   then -(intDoParse readDec xs)
                                   else   intDoParse readDec s
parseDecInt _ = error parseIntErrorMsg

parseHexInt :: HexInt -> Integer
parseHexInt (HexInt ('0':'x':xs)) = intDoParse readHex xs
parseHexInt _ = error parseIntErrorMsg

parseOctInt :: OctInt -> Integer
parseOctInt (OctInt ('o':xs)) = intDoParse readOct xs
parseOctInt _ = error parseIntErrorMsg

parseBinInt :: BinInt -> Integer
parseBinInt (BinInt ('b':xs)) = intDoParse readBin xs
parseBinInt _ = error parseIntErrorMsg

-- Find undefined variables for a given dependencies map
hasUndefined :: M.Map Text (S.Set Text) -> Maybe (S.Set Text)
hasUndefined deps =
  let ns = M.keysSet deps
      free = S.unions (M.elems deps)
      undef = free S.\\ ns
   in if not (null undef)
         then Just undef
         else Nothing

-- Given a map of names to their dependencies return either a list of names
-- that are part of cycles or a list of names that specify the (reverse) order
-- in which the names should be processed
findCycles :: M.Map Text (S.Set Text) -> Either [Text] [Text]
findCycles m =
  let (dg, vertToNode, _) = G.graphFromEdges $ map (\(a,b)->(a,a,b))
                                             $ M.toList (M.map S.toList m)
      nodeName = (\(a,_,_)->a) . vertToNode
      (lb, ub) = A.bounds dg
      -- For each node, find it's successors and determine if it has a path back
      recDeps = [nodeName x | x <- [lb..ub]
                            , y <- dg A.! x
                            , G.path dg y x]
   in if not (null recDeps)
         then Left recDeps
         else Right . map nodeName . G.topSort $ dg

lowerFirst :: Text -> Text
lowerFirst xs = case uncons xs of
                     Just (x, xs') -> (toLower . singleton $ x) <> xs'
                     Nothing ->
                       error "Compiler error. Struct name should not be empty."

