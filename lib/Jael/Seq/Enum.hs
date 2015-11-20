module Jael.Seq.Enum where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.UserDefTy
import Jael.Util
import Jael.Seq.HM_Types

data Tag = Tag Text
         | TagWithTy Text Ty
           deriving Show

data Enumer = Enumer (NE.NonEmpty Tag)
  deriving (Show)

data EnumerDefError = EDEDupTags [Text]
  deriving (Eq, Show)

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
validateEnumer (Enumer fs) =
  let (tags, _) = splitTags . NE.toList $ fs
      dups = repeated tags
   in if null dups
         then Nothing
         else Just $ EDEDupTags dups

enumEnvItems :: (Text, Enumer) -> [(Text, PolyTy)]
enumEnvItems (n, Enumer fs) =
  let enumTy = TNamed n []
      nLowered = lowerFirst n
  in  map (\t -> case t of
                      Tag tn -> (nLowered <> "::" <> tn, PolyTy [] enumTy)
                      TagWithTy tn ty -> ( nLowered <> "::" <> tn
                                         , PolyTy [] $ TFun ty enumTy)
          ) (NE.toList fs)

enumTypeDeps :: Enumer -> S.Set Text
enumTypeDeps (Enumer fs) = S.fromList $ mapMaybe
  (\tag -> case tag of
                (TagWithTy _ (TNamed n _)) -> Just n
                _ -> Nothing
  ) $ NE.toList fs

gToTag :: GTEnumElem -> Tag
gToTag (GTEnumElemNoTy (LIdent t)) = Tag (pack t)
gToTag (GTEnumElemWithTy (LIdent t) ty) = TagWithTy (pack t) (gToType ty)

gToEnumer :: GTEnumDef -> Enumer
gToEnumer (GTEnumDef elems) =
  case NE.nonEmpty elems of
       Nothing -> notEnoughElements 1 "GTEnumDef" "GTEnumElem"
       Just xs -> Enumer (NE.map gToTag xs)

