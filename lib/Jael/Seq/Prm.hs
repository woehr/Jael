module Jael.Seq.Prm where

import Jael.Seq.HM_Types

data Prm = PAdd
         | PSub
         | PTimes
         | PDiv
         | PMod
         | POr
         | PAnd
         | PEq
         | PNeq
         | PGeq
         | PLeq
         | PGt
         | PLt
         | PNot
         | PBitCat
         deriving (Bounded, Enum, Eq)

instance SeqTypable Prm where
  tyOf (PAdd)   = TyFun (TySimple TyInt) (TyFun (TySimple TyInt) (TySimple TyInt))
  tyOf (PSub)   = TyFun (TySimple TyInt) (TyFun (TySimple TyInt) (TySimple TyInt))
  tyOf (PTimes) = TyFun (TySimple TyInt) (TyFun (TySimple TyInt) (TySimple TyInt))
  tyOf (PDiv)   = TyFun (TySimple TyInt) (TyFun (TySimple TyInt) $ TyNamed "IntDivRes" [])
  tyOf (PMod)   = TyFun (TySimple TyInt) (TyFun (TySimple TyInt) (TySimple TyInt))
  tyOf (POr)    = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PAnd)   = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PEq)    = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PNeq)   = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PGeq)   = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PLeq)   = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PGt)    = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PLt)    = TyFun (TySimple TyBool) (TyFun (TySimple TyBool) (TySimple TyBool))
  tyOf (PNot)   = TyFun (TySimple TyBool) (TySimple TyBool)
  tyOf (PBitCat)= TyFun (TySimple TyBit) (TyFun (TySimple TyBit) (TySimple TyBit))

instance Show Prm where
  show (PAdd)    = "+"
  show (PSub)    = "-"
  show (PTimes)  = "*"
  show (PDiv)    = "/"
  show (PMod)    = "%"
  show (POr)     = "||"
  show (PAnd)    = "&&"
  show (PEq)     = "=="
  show (PNeq)    = "!="
  show (PGeq)    = ">="
  show (PLeq)    = "<="
  show (PGt)     = ">"
  show (PLt)     = "<"
  show (PNot)    = "!"
  show (PBitCat) = "#"

allPrm :: [Prm]
allPrm = [minBound .. maxBound]

prmFuncs :: [(Text, PolyTy)]
prmFuncs = map (\p -> (tshow p, polyTy $ tyOf p)) allPrm

readPrm :: Text -> Maybe Prm
readPrm s = case s of
                 "+"  -> Just PAdd
                 "-"  -> Just PSub
                 "*"  -> Just PTimes
                 "/"  -> Just PDiv
                 "%"  -> Just PMod
                 "||" -> Just POr
                 "&&" -> Just PAnd
                 "==" -> Just PEq
                 "!=" -> Just PNeq
                 ">=" -> Just PGeq
                 "<=" -> Just PLeq
                 ">"  -> Just PGt
                 "<"  -> Just PLt
                 "!"  -> Just PNot
                 "#"  -> Just PBitCat
                 _ -> Nothing

readPrmPartial :: Text -> Prm
readPrmPartial s = fromMaybe (error "Expected successful parse of Prm type")
                             (readPrm s)

