{-# Language RecordWildCards #-}

module Jael.CodeGen.Convert where

import qualified Data.Functor.Foldable as F
import           Jael.Seq.CG_AST
import           Jael.Seq.CG_Types
import           Jael.Seq.Literal
--import           Jael.Seq.Prm
import           LLVM.General.AST
import           LLVM.General.AST.Constant

-- Functions for transforming Jael data structures into LLVM

class GenLLVM a where
  type LLVMOutput a
  toLLVM :: a -> LLVMOutput a

instance GenLLVM CGTy where
  type LLVMOutput CGTy = Type
  toLLVM = F.cata alg
    where alg (CGTySimpleF CGUnit) = VoidType
          alg (CGTySimpleF CGInt{..}) = IntegerType
            { typeBits=max (numBitsForInt cgIntMin) (numBitsForInt cgIntMax) }
          alg (CGTySimpleF CGBool) = IntegerType { typeBits=1 }
          alg (CGTySimpleF CGBit{..}) = IntegerType
            { typeBits=fromIntegral cgBitSize }
          alg (CGTyTupF xs) = StructureType { isPacked=False, elementTypes=xs }

instance GenLLVM CGEx where
  type LLVMOutput CGEx = [BasicBlock]
  toLLVM = undefined

instance GenLLVM Literal where
  type LLVMOutput Literal = Constant
  toLLVM (LUnit) = undefined
  toLLVM (LInt  x) = Int { integerBits=numBitsForInt x, integerValue=x }
  toLLVM (LBool True)  = Int { integerBits=1, integerValue= -1 }
  toLLVM (LBool False) = Int { integerBits=1, integerValue=  0 }
  toLLVM (LBit  x) = Int { integerBits=fromIntegral (length x)
                         , integerValue=bitsToInteger x }

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

bitsToInteger :: [BitValue] -> Integer
bitsToInteger =
  foldr (\(bitNum, bitVal) acc ->
    case bitVal of
         BVSet -> acc + 2^bitNum
         BVNotSet -> acc
  ) 0 . zip [0::Integer ..] . reverse

