module Jael.Seq.Enum where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.UserDefTy
import Jael.Util
import Jael.Seq.Types

data Tag = Tag Text
         | TagWithTy Text Ty
           deriving Show

data Enumer = Enumer [Text] (NE.NonEmpty Tag)
  deriving (Show)

data EnumerDefError = EnumerDefError
  { eErrDupTv    :: S.Set Text
  , eErrDupField :: S.Set Text
  , eErrFreeTv   :: S.Set Text
  , eErrUnusedTv :: S.Set Text
  } deriving (Eq, Show)

instance UserDefTy Enumer where
  type TGrammar Enumer = GTEnumDef
  type TError   Enumer = EnumerDefError
  type TEnvItem Enumer = PolyTy

  gToUserDefTy = gToEnumer
  validate = validateEnumer
  typeDeps = enumTypeDeps
  envItems = enumEnvItems

splitTags :: [Tag] -> ([Text], [Ty])
splitTags ts = let xs = map splitTag ts
                in (map fst xs, mapMaybe snd xs)

splitTag :: Tag -> (Text, Maybe Ty)
splitTag t = case t of
                  Tag tn -> (tn, Nothing)
                  TagWithTy tn ty -> (tn, Just ty)

validateEnumer :: Enumer -> Maybe EnumerDefError
validateEnumer (Enumer tvs fs) =
  let (tags, tys) = splitTags . NE.toList $ fs
  in maybe Nothing (\(t, u, v, w) ->
                       Just EnumerDefError
                         { eErrDupTv = t
                         , eErrDupField = u
                         , eErrFreeTv = v
                         , eErrUnusedTv = w
                         }
                   )
        $ checkDefErr tvs tags (typeVars'' tys)

enumEnvItems :: (Text, Enumer) -> [(Text, PolyTy)]
enumEnvItems (n, Enumer tvs fs) =
  let enumTy = TNamed n $ map TyVar tvs
  in  map (\t -> case t of
                      Tag tn -> (n <> "::" <> tn, PolyTy tvs enumTy)
                      TagWithTy tn ty -> ( n <> "::" <> tn
                                         , PolyTy tvs $ TFun ty enumTy)
          ) (NE.toList fs)

enumTypeDeps :: Enumer -> S.Set Text
enumTypeDeps (Enumer _ fs) = S.fromList $ mapMaybe
  (\(TagWithTy _ ty) -> case ty of
                             TNamed n _ -> Just n
                             _ -> Nothing
  ) $ NE.toList fs

gToTag :: GTEnumElem -> Tag
gToTag (GTEnumElemNoTy (LIdent t)) = Tag (pack t)
gToTag (GTEnumElemWithTy (LIdent t) ty) = TagWithTy (pack t) (gToType ty)

gToEnumer :: GTEnumDef -> Enumer
gToEnumer (GTEnumDef tvs elems) =
  case NE.nonEmpty elems of
       Nothing -> notEnoughElements 1 "GTEnumDef" "GTEnumElem"
       Just xs -> Enumer (map (\(GTVars (LIdent s)) -> pack s) tvs)
                         (NE.map gToTag xs)

