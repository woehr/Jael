{-# Language NoImplicitPrelude #-}
{-# Language TypeFamilies #-}

module Jael.Seq.Enum where

import ClassyPrelude hiding (Enum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.UserDefTy
import Jael.Util
import Jael.Seq.Types

data Tag = Tag Text
         | TagWithTy Text Ty
           deriving Show

data Enum = Enum [Text] (NE.NonEmpty Tag)
  deriving (Show)

data EnumDefError = EnumDefError
  { eErrDupTv    :: S.Set Text
  , eErrDupField :: S.Set Text
  , eErrFreeTv   :: S.Set Text
  , eErrUnusedTv :: S.Set Text
  } deriving (Eq, Show)

instance UserDefTy Enum where
  type TGrammar Enum = GTEnumDef
  type TError   Enum = EnumDefError
  type TEnvItem Enum = PolyTy

  gToUserDefTy = gToEnum
  validate = validateEnum
  typeDeps = enumTypeDeps
  envItems = enumEnvItems

splitTags :: [Tag] -> ([Text], [Ty])
splitTags ts = let xs = map splitTag ts
                in (map fst xs, mapMaybe snd xs)

splitTag :: Tag -> (Text, Maybe Ty)
splitTag t = case t of
                  Tag tn -> (tn, Nothing)
                  TagWithTy tn ty -> (tn, Just ty)

validateEnum :: Enum -> Maybe EnumDefError
validateEnum (Enum tvs fs) =
  let (tags, tys) = splitTags . NE.toList $ fs
  in maybe Nothing (\(t, u, v, w) ->
                       Just EnumDefError
                         { eErrDupTv = t
                         , eErrDupField = u
                         , eErrFreeTv = v
                         , eErrUnusedTv = w
                         }
                   )
        $ checkDefErr tvs tags (typeVars'' tys)

enumEnvItems :: (Text, Enum) -> [(Text, PolyTy)]
enumEnvItems (n, (Enum tvs fs)) =
  let enumTy = TNamed n $ map TVar tvs
  in  map (\t -> case t of
                      Tag tn -> (n ++ "::" ++ tn, PolyTy tvs enumTy)
                      TagWithTy tn ty -> ( n ++ "::" ++ tn
                                         , PolyTy tvs $ TFun ty enumTy)
          ) (NE.toList fs)

enumTypeDeps :: Enum -> S.Set Text
enumTypeDeps (Enum _ fs) = S.fromList $ mapMaybe
  (\(TagWithTy _ ty) -> case ty of
                             TNamed n _ -> Just n
                             _ -> Nothing
  ) $ NE.toList fs

gToTag :: GTEnumElem -> Tag
gToTag (GTEnumElemNoTy (LIdent t)) = Tag (pack t)
gToTag (GTEnumElemWithTy (LIdent t) ty) = TagWithTy (pack t) (gToType ty)

gToEnum :: GTEnumDef -> Enum
gToEnum (GTEnumDef tvs elems) =
  case NE.nonEmpty elems of
       Nothing -> notEnoughElements 1 "GTEnumDef" "GTEnumElem"
       Just xs -> Enum (map (\(GTVars (LIdent s)) -> pack s) tvs)
                       (map gToTag xs)

