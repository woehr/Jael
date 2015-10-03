{-# Language NoImplicitPrelude #-}
{-# Language TypeFamilies #-}

module Jael.Seq.Struct where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.UserDefTy
import Jael.Util
import Jael.Seq.Types

type Field = (Text, Ty)

data Struct = Struct [Text] (NE.NonEmpty Field)

data StructDefError = StructDefError
  { sErrDupTv    :: S.Set Text
  , sErrDupField :: S.Set Text
  , sErrFreeTv   :: S.Set Text
  , sErrUnusedTv :: S.Set Text
  } deriving (Eq, Show)

instance UserDefTy Struct where
  type TGrammar Struct = GTStructDef
  type TError   Struct = StructDefError
  type TEnvItem Struct = PolyTy

  gToUserDefTy = gToStruct
  validate = validateStruct
  typeDeps = structTypeDeps
  envItems = structEnvItems

validateStruct :: Struct -> Maybe StructDefError
validateStruct (Struct tvs fs) =
  maybe Nothing (\(t, u, v, w) ->
                     Just StructDefError
                       { sErrDupTv = t
                       , sErrDupField = u
                       , sErrFreeTv = v
                       , sErrUnusedTv = w
                       }
                )
        $ checkDefErr tvs (NE.toList $ map fst fs)
                          (typeVars'' . NE.toList $ map snd fs)

structEnvItems :: (Text, Struct) -> [(Text, PolyTy)]
structEnvItems (n, (Struct tvs fs)) =
  let structTy = TNamed n (map TVar tvs)
      consTy = (foldr (\(_, ft) t -> TFun ft t) structTy fs)
  in (lowerFirst n, PolyTy tvs consTy)
     : map (\(f, t) -> (n ++ "::" ++ f, PolyTy tvs $ TFun structTy t)
           ) (NE.toList fs)

structTypeDeps :: Struct -> S.Set Text
structTypeDeps (Struct _ fs) = S.fromList
  (mapMaybe (\(_, ty) -> case ty of
                              TNamed n _ -> Just n
                              _ -> Nothing
            )
            $ NE.toList fs
  )

lowerFirst :: Text -> Text
lowerFirst xs = case uncons xs of
                     Just (x, xs') -> (toLower . singleton $ x) ++ xs'
                     Nothing ->
                       error "Compiler error. Struct name should not be empty."

gToField :: GTStructElement -> Field
gToField (GTStructElement (GTStructFieldName (LIdent gfn)) gt) =
                                     (pack gfn, gToType gt)

gToStruct :: GTStructDef -> Struct
gToStruct (GTStructDef tvs fields) =
  case NE.nonEmpty fields of
    Nothing -> notEnoughElements 1 "GTStructElement" "GTStructDef"
    Just xs -> Struct (map (\(GTVars (LIdent s)) -> pack s) tvs)
                      (map gToField xs)

