{-#Language NoImplicitPrelude #-}

module Jael.Seq.AlgDataTy where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.AST
import Jael.Seq.Types

type Field = (Text, Ty)
data Struct = Struct Text [Text] (NE.NonEmpty Field)
              deriving Show

data Tag = Tag Text
         | TagWithTy Text Ty
           deriving Show

data Enumer = Enumer Text [Text] (NE.NonEmpty Tag)
              deriving Show

class Show a => AlgDataTy a where
  validateAdt :: a -> Either TDefError [(Text, PolyTy)]
  adtTy :: a -> Ty

splitTags :: [Tag] -> ([Text], [Ty])
splitTags ts = let xs = map splitTag ts
                in (map fst xs, mapMaybe snd xs)

splitTag :: Tag -> (Text, Maybe Ty)
splitTag t = case t of
                  Tag tn -> (tn, Nothing)
                  TagWithTy tn ty -> (tn, Just ty)

instance AlgDataTy Enumer where
  validateAdt x@(Enumer n tvs ts) =
    case uncurry (checkDefErr tvs) (splitTags . NE.toList $ ts) of
         Just err -> Left err
         Nothing  ->
           Right $ map (
                     \t -> case t of
                                Tag tn -> (n ++ "::" ++ tn, PolyTy tvs $ adtTy x)
                                TagWithTy tn ty -> (n ++ "::" ++ tn, PolyTy tvs $ TFun ty $ adtTy x)
                   ) (NE.toList ts)


  adtTy (Enumer n tvs _) = TNamed n (map TVar tvs)

instance AlgDataTy Struct where
  validateAdt x@(Struct n tvs fs) =
    case uncurry (checkDefErr tvs) (unzip . NE.toList $ fs) of
         Just err -> Left err
         Nothing  ->
           Right $ (lowerFirst n, PolyTy tvs $ structConsTy x) :
             map (
               \(f, t) -> (n ++ "::" ++ f, PolyTy tvs $ TFun (adtTy x) t)
             ) (NE.toList fs)

  adtTy (Struct n tvs _) = TNamed n (map TVar tvs)

lowerFirst :: Text -> Text
lowerFirst xs = case uncons xs of
                     Just (x, xs') -> (toLower . singleton $ x) ++ xs'
                     Nothing ->
                       error "Compiler error. Struct name should not be empty."

structConsTy :: Struct -> Ty
structConsTy s@(Struct _ _ fs) =
  foldr (\(_, ft) t -> TFun ft t) (adtTy s) fs

tysToTyVars :: [Ty] -> [Text]
tysToTyVars [] = []
tysToTyVars (t:ts) = case t of
                          (TVar x) -> x:tysToTyVars ts
                          _        -> tysToTyVars ts

-- Given a list of text representing ty vars and a list representing field names
-- and their associated type, check for type definition errors
checkDefErr :: [Text] -> [Text] -> [Ty] -> Maybe TDefError
checkDefErr tvs fs ts =
  let dupTVs = repeated tvs
      dupFields = repeated fs
      declaredTVs = S.fromList tvs
      usedTVs = S.fromList . tysToTyVars $ ts
      freeTVs = S.toList $ usedTVs `S.difference` declaredTVs
      unusedTVs = S.toList $ declaredTVs `S.difference` usedTVs
   in if (not . null $ dupTVs) ||
         (not . null $ dupFields) ||
         (not . null $ freeTVs) ||
         (not . null $ unusedTVs)
         then Just $ TDefError (DuplicateTyVars dupTVs)
                               (DuplicateFields dupFields)
                               (FreeTyVars freeTVs)
                               (UnusedTyVars unusedTVs)
         else Nothing

gToTVars :: [GTVars] -> [Text]
gToTVars = map (\(GTVars (LIdent s)) -> pack s)

gToField :: GTStructElement -> Field
gToField (GTStructElement (GTStructFieldName (LIdent gfn)) gt) =
                                     (pack gfn, gToType gt)

gToStruct :: GTStructDef -> Struct
gToStruct (GTStructDef (UIdent n) tvs fields) =
  case NE.nonEmpty fields of
    Nothing -> notEnoughElements 1 "GTStructElement" "GTStructDef"
    Just xs -> Struct (pack n) (gToTVars tvs) (map gToField xs)

gToTag :: GTEnumElem -> Tag
gToTag (GTEnumElemNoTy (LIdent t)) = Tag (pack t)
gToTag (GTEnumElemWithTy (LIdent t) ty) = TagWithTy (pack t) (gToType ty)

gToEnumer :: GTEnumDef -> Enumer
gToEnumer (GTEnumDef (UIdent n) tvs elems) =
  case NE.nonEmpty elems of
       Nothing -> notEnoughElements 1 "GTEnumDef" "GTEnumElem"
       Just xs -> Enumer (pack n) (gToTVars tvs) (map gToTag xs)

