{-# Language RecordWildCards #-}

module Jael.Seq.UserDefinedType where

import qualified Data.Set as S
import           Jael.Grammar
import           Jael.Seq.Types
import           Jael.Util

data UserDefinedTypeErr = UDTDuplicateFieldsErr (S.Set Text)
                        deriving (Eq, Show)

data UserDefinedType = UDTStruct { udtsFields :: [(Text, S1Ty)]
                                 }
                     | UDTEnum   { udteTags :: [(Text, S1Ty)]
                                 }
                     | UDTData -- unimplemented
                     deriving (Eq, Show)

validateUDT :: UserDefinedType -> Maybe UserDefinedTypeErr
validateUDT (UDTEnum{..}) =
  let tags = map fst udteTags
      dups = repeated tags
   in if null dups
         then Nothing
         else Just $ UDTDuplicateFieldsErr $ S.fromList dups
validateUDT (UDTStruct{..}) =
  let dups = repeated . map fst $ udtsFields
   in if null dups
         then Nothing
         else Just $ UDTDuplicateFieldsErr $ S.fromList dups
validateUDT (UDTData) = Nothing

typeDependencies :: UserDefinedType -> S.Set Text
typeDependencies x =
  let mergeDeps ts = S.unions $ map (s1TyDepends . snd) ts
   in case x of
           (UDTEnum{..}) -> mergeDeps udteTags
           (UDTStruct{..}) -> mergeDeps udtsFields
           (UDTData) -> S.empty

seqEnvItems :: (Text, UserDefinedType) -> [(Text, HMPolyTy)]
seqEnvItems (_, UDTEnum{..}) = []
seqEnvItems (_, UDTStruct{..}) = []
seqEnvItems (_, UDTData) = []

gToField :: GTStructElement -> (Text, S1Ty)
gToField (GTStructElement (LIdent gfn) gt) = (pack gfn, gToType gt)
gToField (GTStructElementAnn _ (LIdent gfn) gt) = (pack gfn, gToType gt)

gStructToUDT :: GTStructDef -> UserDefinedType
gStructToUDT (GTStructDef fields) =
  case fields of
       [] -> error "Grammar should ensure at least 1 field in a struct."
       xs -> UDTStruct $ map gToField xs

gEnumToUDT :: GTEnumDef -> UserDefinedType
gEnumToUDT (GTEnumDef elems) =
  case elems of
       [] -> error "Grammar should ensure at least 1 field in an enum."
       xs -> UDTEnum $ map gToTag xs

gToTag :: GTEnumElem -> (Text, S1Ty)
gToTag (GTEnumElem (LIdent t) ty) = (pack t, gToType ty)
