{-# LANGUAGE NoImplicitPrelude #-}

module Jael.AST0 where

import ClassyPrelude
import qualified Data.Map as M

import Jael.Grammar

-- AST0 represents the top level definitions of a program by mapping
-- their name to their type and expression.
type AST0 = Map Text (Ty, Ex)
-- Perhaps this signature when adding poly-types?
--type AST0 = Map Text (Either PolyTy Ty, Ex)

data Lit  = LInt Integer
          | LFloat Double
            deriving (Show)

data Ex = EVar Text
        | ELit Lit
        | EApp Ex Ex
        | EAbs Text Ex
        | ELet Text Ex Ex
          deriving (Show)

data Ty = TVar Text
        | TInt
        | TFloat
        | TChar
        | TString
        | TBool
        | TFun Ty Ty
          deriving (Show)

--data PolyTy = PolyTy [Text] Ty
--              deriving (Show)

toAST0 :: GProg -> Either Text AST0
toAST0 (GProg fns) =
  let tyOf :: GType -> Ty
      tyOf t = case t of
                    GTyInt   -> TInt
                    GTyFloat -> TFloat
                    GTyChar  -> TChar
                    GTyStr   -> TString
                    --TyBool -> TBool

      -- Helper to convert grammar to AST0 function type
      mkTy :: [GArg] -> GType -> Ty
      mkTy (a@(GArgTy argId argTy):as) retTy = TFun (tyOf argTy) (mkTy as retTy)
      mkTy []     retTy = tyOf retTy

      -- Helper to conver grammar to AST0 function expression
      mkEx :: [GLet] -> GExpr -> Ex
      mkEx (l@(GLet (Ident letId) letEx):ls) e = ELet (pack letId) (mkEx [] letEx) (mkEx ls e)
      mkEx [] e = case e of
                       GEVar    (Ident i) -> EVar (pack i)

                       GEInt    i     -> ELit (LInt  i)
                       GEIntNeg i     -> ELit (LInt (-i))

--                       GEChar   c
--                       GEStr    s

                       GEPlus   e1 e2 -> EApp (EApp (EVar "+") (mkEx [] e1)) (mkEx [] e2)
                       GEMinus  e1 e2 -> EApp (EApp (EVar "-") (mkEx [] e1)) (mkEx [] e2)
                       GETimes  e1 e2 -> EApp (EApp (EVar "*") (mkEx [] e1)) (mkEx [] e2)
      
      -- Add a function to the symbol map if valid
      addSym :: AST0 -> GFunc -> Either Text AST0
      addSym acc (GFunc (Ident id) args ret lets expr) =
        let argsMissingTypes = not . and . map (\a -> case a of
                                                        GArgTy   _ _ -> True
                                                        GArgUnty _   -> False
                                               ) $ args
            alreadyDefined = member (pack id) acc
            insertInto = M.insert (pack id) ((mkTy args ret), (mkEx lets expr))
        in  if      alreadyDefined   then Left $
                      "Function \"" ++ pack id ++ "\" defined multiple times"
            else if argsMissingTypes then Left $
                      "Function \"" ++ pack id ++ "\" missing argument types"
            else    Right $ insertInto acc

  in  (foldM addSym M.empty fns) >>= checkMain

  where
      checkMain :: AST0 -> Either Text AST0
      checkMain a = if member "main" a
                       then Right a
                       else Left  "No main function"
