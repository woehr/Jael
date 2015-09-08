{-#Language NoImplicitPrelude #-}

module Jael.Seq.UserDefTy where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.Types

type Field = (Text, Ty)

data Tag = Tag Text
         | TagWithTy Text Ty
           deriving Show

data UserDefTy = Struct Text [Text] (NE.NonEmpty Field)
               | Enumer Text [Text] (NE.NonEmpty Tag)
               deriving (Show)

data TDefError = TDefError
  { dupTv    :: [Text]
  , dupField :: [Text]
  , freeTv   :: [Text]
  , unusedTv :: [Text]
  } deriving (Show)

splitTags :: [Tag] -> ([Text], [Ty])
splitTags ts = let xs = map splitTag ts
                in (map fst xs, mapMaybe snd xs)

splitTag :: Tag -> (Text, Maybe Ty)
splitTag t = case t of
                  Tag tn -> (tn, Nothing)
                  TagWithTy tn ty -> (tn, Just ty)

validateType :: UserDefTy -> Either TDefError [(Text, PolyTy)]
validateType (Enumer n tvs fs) =
  let enumTy = TNamed n $ map TVar tvs
   in case uncurry (checkDefErr tvs) (splitTags . NE.toList $ fs) of
           Just err -> Left err
           Nothing  ->
             Right $ map (
                       \t -> case t of
                                  Tag tn -> (n ++ "::" ++ tn, PolyTy tvs enumTy)
                                  TagWithTy tn ty -> ( n ++ "::" ++ tn
                                                     , PolyTy tvs $ TFun ty enumTy)
                     ) (NE.toList fs)

validateType (Struct n tvs fs) =
  let structTy = TNamed n (map TVar tvs)
      consTy = (foldr (\(_, ft) t -> TFun ft t) structTy fs)
   in case uncurry (checkDefErr tvs) (unzip . NE.toList $ fs) of
           Just err -> Left err
           Nothing  ->
             Right $ (lowerFirst n, PolyTy tvs consTy) :
               map (
                 \(f, t) -> (n ++ "::" ++ f, PolyTy tvs $ TFun structTy t)
               ) (NE.toList fs)

lowerFirst :: Text -> Text
lowerFirst xs = case uncons xs of
                     Just (x, xs') -> (toLower . singleton $ x) ++ xs'
                     Nothing ->
                       error "Compiler error. Struct name should not be empty."


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
         then Just $ TDefError
           { dupTv = dupTVs
           , dupField = dupFields
           , freeTv = freeTVs
           , unusedTv = unusedTVs
           }
         else Nothing

gToTVars :: [GTVars] -> [Text]
gToTVars = map (\(GTVars (LIdent s)) -> pack s)

gToField :: GTStructElement -> Field
gToField (GTStructElement (GTStructFieldName (LIdent gfn)) gt) =
                                     (pack gfn, gToType gt)

gToStruct :: GTStructDef -> UserDefTy
gToStruct (GTStructDef (UIdent n) tvs fields) =
  case NE.nonEmpty fields of
    Nothing -> notEnoughElements 1 "GTStructElement" "GTStructDef"
    Just xs -> Struct (pack n) (gToTVars tvs) (map gToField xs)

gToTag :: GTEnumElem -> Tag
gToTag (GTEnumElemNoTy (LIdent t)) = Tag (pack t)
gToTag (GTEnumElemWithTy (LIdent t) ty) = TagWithTy (pack t) (gToType ty)

gToEnumer :: GTEnumDef -> UserDefTy
gToEnumer (GTEnumDef (UIdent n) tvs elems) =
  case NE.nonEmpty elems of
       Nothing -> notEnoughElements 1 "GTEnumDef" "GTEnumElem"
       Just xs -> Enumer (pack n) (gToTVars tvs) (map gToTag xs)

