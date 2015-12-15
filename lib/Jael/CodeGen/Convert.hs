module Jael.CodeGen.Convert where

import qualified Data.Functor.Foldable as F
import           Jael.Seq.AST
import           Jael.Seq.Types
import           Jael.Seq.Literal
--import           Jael.Seq.Prm
import           LLVM.General.AST
import           LLVM.General.AST.Constant

-- Functions for transforming Jael data structures into LLVM

class GenLLVM a where
  type LLVMOutput a
  toLLVM :: a -> LLVMOutput a

instance GenLLVM S2Ty where
  type LLVMOutput S2Ty = Type
  toLLVM = F.cata alg
    where alg (S2TySimpleF BTUnit) = VoidType
          alg (S2TySimpleF BTBit{..}) = IntegerType
            { typeBits=fromIntegral btBits }
          alg (S2TySimpleF BTBool) = IntegerType { typeBits=1 }
          alg (S2TySimpleF BTBuffer{..}) = undefined
          alg (S2TySimpleF BTInt{..}) = IntegerType
            { typeBits=max (numBitsForInt btIntMin) (numBitsForInt btIntMax) }
          alg (S2TyTupF xs) = StructureType { isPacked=False, elementTypes=xs }
          alg (S2TyNamedF _ _) = undefined

instance GenLLVM S2TyEx where
  type LLVMOutput S2TyEx = [BasicBlock]
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

