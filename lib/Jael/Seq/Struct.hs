{-# Language NoImplicitPrelude #-}

module Jael.Seq.Struct
( Struct(..)
, StructError(..)
, DuplicateTyVars(..)
, DuplicateFields(..)
, FreeTyVars(..)
, UnusedTyVars(..)
, gToStruct
, validateStruct
) where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.AST
import Jael.Seq.Types

type SElement = (Text, Ty)

data Struct = Struct Text [Text] (NE.NonEmpty SElement)

newtype DuplicateTyVars = DuplicateTyVars [Text]
  deriving Show

newtype DuplicateFields = DuplicateFields [Text]
  deriving Show

newtype FreeTyVars = FreeTyVars (S.Set Text)
  deriving Show

newtype UnusedTyVars = UnusedTyVars (S.Set Text)
  deriving Show

data StructError = StructError DuplicateTyVars
                               DuplicateFields
                               FreeTyVars
                               UnusedTyVars
                   deriving Show

lowerFirst :: Text -> Text
lowerFirst xs = case uncons xs of
                     Just (x, xs') -> (toLower . singleton $ x) ++ xs'
                     Nothing ->
                       error "Compiler error. Struct name should not be empty."

structTy :: Struct -> Ty
structTy (Struct n tvs _)  = TNamed n (map TVar tvs)

constructorTy :: Struct -> Ty
constructorTy s@(Struct _ _ fs) =
  foldr (\(fn, ft) t -> TFun ft t) (structTy s) fs

tysToTyVars :: [Ty] -> [Text]
tysToTyVars [] = []
tysToTyVars (t:ts) = case t of
                          (TVar x) -> x:tysToTyVars ts
                          _        -> tysToTyVars ts

validateStruct :: Struct -> Either StructError [(Text, PolyTy)]
validateStruct s@(Struct n tvs fs) =
  let dupTVs = repeated tvs
      dupFields = repeated $ NE.toList (map fst fs)
      declaredTVs = S.fromList tvs
      usedTVs = S.fromList . tysToTyVars . NE.toList $ map snd fs
      freeTVs = usedTVs `S.difference` declaredTVs
      unusedTVs = declaredTVs `S.difference` usedTVs
   in if (not . null $ dupTVs) ||
         (not . null $ dupFields) ||
         (not . null $ freeTVs) ||
         (not . null $ unusedTVs)
         then Left $ StructError (DuplicateTyVars dupTVs)
                                 (DuplicateFields dupFields)
                                 (FreeTyVars freeTVs)
                                 (UnusedTyVars unusedTVs)
         else
           let sTy = structTy s
               -- Do something about the following unwieldy line
            in Right $ (lowerFirst n, PolyTy tvs $ constructorTy s):concatMap (\((f, t), i) -> zip [n ++ "::" ++ tshow i, n ++ "::" ++ f] (replicate 2 (PolyTy tvs $ TFun sTy t))) (zip (NE.toList fs) [0..])

gToSElement :: GTStructElement -> SElement
gToSElement (GTStructElement gt (GTStructFieldName (LIdent gfn))) =
                                     (pack gfn, gToType gt)

gToTVars :: [GTStructVars] -> [Text]
gToTVars = map (\(GTStructVars (LIdent s)) -> pack s)

gToStruct :: GTStructDef -> Struct
gToStruct (GTStructDef (UIdent n) tvs xs) =
  case NE.nonEmpty xs of
    Nothing -> notEnoughElements
                 1
                 "GTStructElement"
                 "GTStructDef"
    Just ys -> Struct
                 (pack n)
                 (gToTVars tvs)
                 (map gToSElement ys)

