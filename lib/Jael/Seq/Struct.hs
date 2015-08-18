{-# Language NoImplicitPrelude #-}

module Jael.Seq.Struct
where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar
import Jael.Util
import Jael.Seq.AST
import Jael.Seq.Types

type SElement = (Text, Ty)
data Struct = Struct [Text] (NE.NonEmpty SElement)

type Accessors = M.Map Text Integer

addIfUnique :: Accessors -> (Text, Integer) -> Either Text Accessors
addIfUnique m (k, v) = case M.insertLookupWithKey (\_ n _ -> n) k v m of
                            (Nothing, m') -> Right m'
                            (_, _) -> Left $ "Non-unique accessor " ++ k

mkAccessorMap :: NE.NonEmpty Text -> Either Text Accessors
mkAccessorMap xs = liftA snd $ foldM (\(i, m) x ->
                                       liftA ((,) $ i+1) $ addIfUnique m (x, i)
                                     ) (0, M.empty) xs

parseStruct :: Struct -> Either Text (NE.NonEmpty Ty, Accessors)
parseStruct (Struct n xs) = liftA ((,) $ map snd xs) (mkAccessorMap $ map fst xs)

addStructToEnv :: Struct -> TyEnv -> Either Text TyEnv
addStructToEnv = undefined

builtinStructs :: M.Map Text Struct
builtinStructs = M.fromList [ ("IntDivRes", Struct [] $ NE.fromList
                                [ ("quot", TInt)
                                , ("rem", TInt)
                                ]
                              )
                            ]

gToSElement :: GTStructElement -> SElement
gToSElement (GTStructElement (GTStructFieldName (LIdent gfn)) gt) =
                                     (pack gfn, gToType gt)

gToTVars :: [GTStructVars] -> [Text]
gToTVars gvs = let cs = map (\(GTStructVars (LIdent s)) -> pack s) gvs
                in if length cs == length (S.fromList cs)
                      then cs
                      else error "Duplicate type variables"


gToStruct :: GTStructDef -> Struct
gToStruct (GTStructDef tvs xs) = case NE.nonEmpty xs of
                                  Nothing -> notEnoughElements 1 "GTStructElement" "GTStructDef"
                                  Just ys -> Struct (gToTVars tvs) (map gToSElement ys)

