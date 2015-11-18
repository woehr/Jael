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
         deriving (Enum, Eq)

instance SeqTypable Prm where
  tyOf (PAdd)   = TFun TInt (TFun TInt TInt)
  tyOf (PSub)   = TFun TInt (TFun TInt TInt)
  tyOf (PTimes) = TFun TInt (TFun TInt TInt)
  tyOf (PDiv)   = TFun TInt (TFun TInt $ TNamed "IntDivRes" [])
  tyOf (PMod)   = TFun TInt (TFun TInt TInt)
  tyOf (POr)    = TFun TBool (TFun TBool TBool)
  tyOf (PAnd)   = TFun TBool (TFun TBool TBool)
  tyOf (PEq)    = TFun TBool (TFun TBool TBool)
  tyOf (PNeq)   = TFun TBool (TFun TBool TBool)
  tyOf (PGeq)   = TFun TBool (TFun TBool TBool)
  tyOf (PLeq)   = TFun TBool (TFun TBool TBool)
  tyOf (PGt)    = TFun TBool (TFun TBool TBool)
  tyOf (PLt)    = TFun TBool (TFun TBool TBool)
  tyOf (PNot)   = TFun TBool TBool
  tyOf (PBitCat)= TFun TBit (TFun TBit TBit)

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
allPrm = [PAdd ..]

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

