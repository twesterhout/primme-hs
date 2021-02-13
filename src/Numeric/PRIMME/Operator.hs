{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.PRIMME.Operator
  ( withOperator,
  )
where

import Control.Exception.Safe (bracket)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr
import Foreign.Ptr (FunPtr, castPtr, freeHaskellFunPtr)
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Numeric.PRIMME.Context
import Numeric.PRIMME.Types

C.context (C.baseCtx <> primmeCtx)
C.include "<primme.h>"
C.include "<stdint.h>"

foreign import ccall "wrapper"
  mkCmatrixMatvec :: CmatrixMatvec -> IO (FunPtr CmatrixMatvec)

withCmatrixMatvec :: CmatrixMatvec -> (FunPtr CmatrixMatvec -> IO a) -> IO a
withCmatrixMatvec f = bracket (mkCmatrixMatvec f) freeHaskellFunPtr

withOperator :: forall a b. BlasDatatype a => PrimmeOperator a -> (FunPtr CmatrixMatvec -> IO b) -> IO b
withOperator !f = withCmatrixMatvec cWrapper
  where
    cWrapper :: CmatrixMatvec
    cWrapper xPtr xStridePtr yPtr yStridePtr blockSizePtr params errPtr = do
      xStride <- fromIntegral <$> peek xStridePtr
      yStride <- fromIntegral <$> peek yStridePtr
      blockSize <- fromIntegral <$> peek blockSizePtr
      n <- fromIntegral <$> [CU.exp| PRIMME_INT { $(primme_params* params)->n } |]
      x <-
        Block (n, blockSize) xStride
          <$> flip V.unsafeFromForeignPtr0 (xStride * blockSize)
          <$> newForeignPtr_ (castPtr xPtr)
      y <-
        MBlock (n, blockSize) yStride
          <$> flip MV.unsafeFromForeignPtr0 (yStride * blockSize)
          <$> newForeignPtr_ (castPtr yPtr)
      f x y
      poke errPtr 0
