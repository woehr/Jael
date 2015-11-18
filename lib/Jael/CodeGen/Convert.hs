{-# Language RecordWildCards #-}

module Jael.CodeGen.Convert where

import qualified Data.Functor.Foldable as F
import           Jael.Seq.CG_AST
import           Jael.Seq.CG_Types
import           LLVM.General.AST

-- Functions for transforming Jael data structures into LLVM
-- Functions in this module are named gen<Jael data type> and generate an
-- appropriate LLVM data type

genCGTy :: CGTy -> Type
genCGTy = F.cata alg
  where alg (CGTySimpleF CGUnit) = VoidType
        alg (CGTySimpleF CGInt{..}) = IntegerType
          { typeBits=max (numBitsForInt cgIntMin) (numBitsForInt cgIntMax)
          }
        alg (CGTySimpleF CGBool) = undefined
        alg (CGTySimpleF CGBit{..}) = undefined
        alg (CGTyStructF) = undefined
        alg (CGTyEnumerF) = undefined
        alg (CGTyTupF _) = undefined

genCGEx :: CGEx -> [BasicBlock]
genCGEx = undefined

numBitsForInt :: Integer -> Word32
numBitsForInt x
  | x  > 0 = fromIntegral
              . (+ 2) -- Extra bit for a sign bit
              . (floor :: Double -> Integer)
              . logBase (2 :: Double)
              . fromIntegral $ x
  | x  < 0 = fromIntegral
              . (+ 1)
              . (ceiling :: Double -> Integer)
              . logBase (2 :: Double)
              . fromIntegral
              . abs $ x
  | otherwise = 1 -- 0 is only remaining case

