module Jael.Seq.Struct where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Jael.Grammar
import Jael.UserDefTy
import Jael.Util
import Jael.Seq.CG_Types
import Jael.Seq.HM_Types

type Field = (Text, Ty)

data Struct = Struct (NE.NonEmpty Field)

data StructDefError = SDEDupFields [Text]
  deriving (Eq, Show)

instance UserDefTy Struct where
  type TGrammar Struct = GTStructDef
  type TError   Struct = StructDefError
  type TEnvItem Struct = PolyTy

  gToUserDefTy = gToStruct
  validate = validateStruct
  typeDeps = structTypeDeps
  envItems = structEnvItems

validateStruct :: Struct -> Maybe StructDefError
validateStruct (Struct fs) =
  let dups = repeated . map fst . NE.toList $ fs
   in if null dups
         then Nothing
         else Just $ SDEDupFields dups

structEnvItems :: (Text, Struct) -> [(Text, PolyTy)]
structEnvItems (n, Struct fs) =
  let structTy = TNamed n []
      consTy = foldr (\(_, ft) t -> TFun ft t) structTy fs
      nLowered = lowerFirst n
  in (nLowered, PolyTy [] consTy)
     : map (\(f, t) -> (nLowered <> "::" <> f, PolyTy [] $ TFun structTy t)
           ) (NE.toList fs)

structTypeDeps :: Struct -> S.Set Text
structTypeDeps (Struct fs) = S.fromList
  (mapMaybe (\(_, ty) -> case ty of
                              TNamed n _ -> Just n
                              _ -> Nothing
            )
            $ NE.toList fs
  )

gToField :: GTStructElement -> Field
gToField (GTStructElement (LIdent gfn) gt) = (pack gfn, (tyOf . gToType) gt)
gToField (GTStructElementAnn _ (LIdent gfn) gt) = (pack gfn, (tyOf . gToType) gt)

gToStruct :: GTStructDef -> Struct
gToStruct (GTStructDef fields) =
  case NE.nonEmpty fields of
    Nothing -> notEnoughElements 1 "GTStructElement" "GTStructDef"
    Just xs -> Struct (NE.map gToField xs)

