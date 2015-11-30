module Jael.Seq.Prm where

import Jael.Seq.Types

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

instance HMTypable Prm where
  hmTyOf (PAdd)   = HMTyFun HMTyInt  (HMTyFun HMTyInt   HMTyInt)
  hmTyOf (PSub)   = HMTyFun HMTyInt  (HMTyFun HMTyInt   HMTyInt)
  hmTyOf (PTimes) = HMTyFun HMTyInt  (HMTyFun HMTyInt   HMTyInt)
  hmTyOf (PDiv)   = HMTyFun HMTyInt  (HMTyFun HMTyInt   (HMTyNamed "IntDivRes" []))
  hmTyOf (PMod)   = HMTyFun HMTyInt  (HMTyFun HMTyInt   HMTyInt)
  hmTyOf (POr)    = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PAnd)   = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PEq)    = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PNeq)   = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PGeq)   = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PLeq)   = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PGt)    = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PLt)    = HMTyFun HMTyBool (HMTyFun HMTyBool  HMTyBool)
  hmTyOf (PNot)   = HMTyFun HMTyBool HMTyBool
  hmTyOf (PBitCat)= HMTyFun HMTyBit  (HMTyFun HMTyBit HMTyBit)

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

prmFuncs :: [(Text, HMPolyTy)]
prmFuncs = map (\p -> (tshow p, polyTy $ hmTyOf p)) allPrm

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

