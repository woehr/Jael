{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, FlexibleInstances #-}

-- Implementation based off of https://github.com/wh5a/Algorithm-W-Step-By-Step

module Jael.AST0 where

import ClassyPrelude
import qualified Control.Monad.State as ST
import qualified Control.Monad.Trans.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import Jael.Grammar

-- AST0 represents the top level definitions of a program by mapping
-- their name to their type and expression.
type AST0 = Map Text (Ty, Ex)
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
          deriving (Eq, Show)

data PolyTy = PolyTy [Text] Ty
              deriving (Show)

type TyEnv = M.Map Text PolyTy

type TySub = M.Map Text Ty

data TIState = TIState { supply :: Integer }

type TI a = ST.StateT TIState (E.EitherT Text Identity) a

returnLeft :: Text -> TI a
returnLeft = lift . E.left

runTI :: TI a -> Either Text a
runTI t = runIdentity $ E.runEitherT $ ST.evalStateT t TIState{ supply = 0}

newTV :: TI Ty
newTV = do
  s <- ST.get
  let count = supply s
  ST.put s{ supply = supply s + 1 }
  return $ TVar ("a" ++ tshow count)

class TyOps a where
  ftv :: a -> S.Set Text
  apply :: TySub -> a -> a

instance TyOps Ty where
  ftv (TVar t)     = S.singleton t
  ftv TInt         = S.empty
  ftv TFloat       = S.empty
  ftv TChar        = S.empty
  ftv TString      = S.empty
  ftv (TFun t1 t2) = S.union (ftv t1) (ftv t2)

  apply s t@(TVar v) =
    case M.lookup v s of
      Nothing   -> t
      Just newt -> newt
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
  apply _ t = t

instance TyOps PolyTy where
  -- Free type variables of a type scheme are the ones not bound by a universal
  -- quantifier. I.e., the type variables within t not in vs
  ftv (PolyTy vs t) = (ftv t) `S.difference` (S.fromList vs)
  -- This first deletes the variables of the scheme from the substitution then
  -- applies the substitution
  apply s (PolyTy vs t) = PolyTy vs (apply (foldr M.delete s vs) t)

instance TyOps a => TyOps [a] where
  ftv ls = foldr S.union S.empty $ map ftv ls
  apply s ls = map (apply s) ls

instance TyOps TyEnv where
  ftv env = ftv $ M.elems env
  apply sub env = M.map (apply sub) env

remove :: TyEnv -> Text -> TyEnv
remove env t = M.delete t env

nullSub :: TySub
nullSub = M.empty

compSub :: TySub -> TySub -> TySub
compSub a b = M.union (M.map (apply a) b) a

-- Creates a scheme from a type by adding the qualified type variables of the
-- environment
generalization :: TyEnv -> Ty -> PolyTy
generalization env t = PolyTy (S.toList $ ftv t `S.difference` ftv env) t

-- Creates a type from a scheme by making new type variables and applying
-- a substituion from the old to the new
instantiation :: PolyTy -> TI Ty
instantiation (PolyTy vs ty) = do
  nvs <- mapM (\_ -> newTV) vs
  return $ apply (M.fromList $ zip vs nvs) ty

-- Most general unifier. Used in the application rule for determining the return
-- type after application to a function
mgu :: Ty -> Ty -> Either Text TySub
mgu (TFun l1 r1) (TFun l2 r2) = do
  sub1 <- mgu l1 l2
  sub2 <- mgu (apply sub1 r1) (apply sub1 r2)
  return $ compSub sub1 sub2
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu TInt    TInt    = Right nullSub
mgu TFloat  TFloat  = Right nullSub
mgu TChar   TChar   = Right nullSub
mgu TString TString = Right nullSub
mgu TBool   TBool   = Right nullSub
mgu t1 t2 = Left $ "Types \"" ++ tshow t1 ++ "\" and \"" ++ tshow t2 ++ "\" do not unify."

varBind :: Text -> Ty -> Either Text TySub
varBind u t
  | t == TVar u        = Right nullSub
  | S.member u (ftv t) = Left $ "Can not bind \"" ++ tshow u ++ "\" to \""
      ++ tshow t ++ "\" because \"" ++ tshow u ++ "\" is a free type variable of \""
      ++ tshow t
  | otherwise          = Right $ M.singleton u t

ti :: TyEnv -> Ex -> TI (TySub, Ty)
-- Literals
ti _ (ELit (LInt _))    = return (nullSub, TInt)
ti _ (ELit (LFloat _))  = return (nullSub, TFloat)
--ti _ (ELit (LChar _))   = return (nullSub, TChar)
--ti _ (ELit (LString _)) = return (nullSub, TString)
--ti _ (ELit (LBool _))   = return (nullSub, TBool)

-- Variables
ti env (EVar v) = do
  case M.lookup v env of
    Nothing -> lift . E.left $ "unbound variable \"" ++ tshow v ++ "\""
    Just sigma -> do
       t <- instantiation sigma
       return (nullSub, t)

-- Function application
ti env (EApp e1 e2) = do
  tv <- newTV
  (sub1, t1) <- ti env e1
  (sub2, t2) <- ti (apply sub1 env) e2
  let sub3 = mgu (apply sub2 t1) (TFun t2 tv)
  case sub3 of
       Left err -> returnLeft (err ++ "\n\n"
                              ++ "Type variable : " ++ tshow tv ++ "\n\n"
                              ++ "Inference 1   : " ++ tshow (t1, sub1) ++ "\n"
                              ++ "   for expr   : " ++ tshow e1 ++ "\n\n"
                              ++ "Inference 2   : " ++ tshow (t2, sub2) ++ "\n"
                              ++ "   for expr   : " ++ tshow e2 ++ "\n\n"
                              )
       Right sub3 -> return (sub3, apply sub3 tv)

-- Abstraction
-- Let

-- TODO: Remove when implemented
ti _ _ = do
  lift . E.left $ "Unimplemented"

typeInference :: TyEnv -> Ex -> TI Ty
typeInference env e = do
  (sub, ty) <- ti env e
  return $ apply sub ty

-- Converts grammar to AST0 but does not verify its correctness
toAST0 :: GProg -> Either Text AST0
toAST0 (GProg fns) =
  let tyOf :: GType -> Ty
      tyOf t = case t of
                    GTyInt   -> TInt
                    GTyFloat -> TFloat
                    GTyChar  -> TChar
                    GTyStr   -> TString
                    --GTyBool -> TBool

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

  in  foldM addSym M.empty fns

