module Numeric.PRIMME.PRIMME (bar) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr
import Foreign.Ptr (FunPtr, Ptr, nullPtr, plusPtr)
import Foreign.Storable
import Numeric.PRIMME.Internal

type MatVecType a = Vector a -> MVector RealWorld a -> IO ()

data PrimmeTarget = PrimmeSmallest

toCprimme_target :: PrimmeTarget -> Cprimme_target
toCprimme_target PrimmeSmallest = Cprimme_smallest

data PrimmeMatrix a = PrimmeMatrix
  { pMatrix :: MatVecType a,
    pDim :: Int
  }

data PrimmeOptions = PrimmeOptions
  { pNumEvals :: Int,
    pTarget :: PrimmeTarget,
    pEps :: Double
  }

withOptions :: PrimmeOptions -> PrimmeMatrix a -> (Cprimme_params -> IO b) -> IO b
withOptions opts matrix func = bracket acquire release worker
  where
    acquire = do
      p <- primme_params_create
      when (p == nullPtr) $ error "failed to allocate primme_params struct"
      return p
    release p = do
      c <- primme_params_destroy p
      when (c /= 0) $ error "failed to destroy primme_params struct"
    worker p = do
      primme_set_dim p (pDim matrix)
      primme_set_num_evals p (pNumEvals opts)
      primme_set_target p (toCprimme_target . pTarget $ opts)
      primme_set_eps p (pEps opts)
      c <- primme_set_method PRIMME_DEFAULT_MIN_MATVECS p
      when (c /= 0) $ error "failed to set method"
      func p

withMatVec :: PrimmeDatatype a => MatVecType a -> (FunPtr CmatrixMatvec -> IO b) -> IO b
withMatVec f = withCmatrixMatvec cWrapper
  where
    cWrapper xPtr xLDimPtr yPtr yLDimPtr blockSizePtr params errPtr = do
      n <- primme_get_dim params
      blockSize <- fromIntegral <$> peek blockSizePtr
      xLDim <- fromIntegral <$> peek xLDimPtr
      yLDim <- fromIntegral <$> peek yLDimPtr
      let mkX i = flip V.unsafeFromForeignPtr0 n <$> newForeignPtr_ (xPtr `plusPtr` (i * xLDim))
          mkY i = flip MV.unsafeFromForeignPtr0 n <$> newForeignPtr_ (yPtr `plusPtr` (i * yLDim))
          go i
            | i < blockSize = do
              x <- mkX i
              y <- mkY i
              f x y
              go (i + 1)
            | otherwise = return ()
      go 0
      poke errPtr 0

-- void (*matrixMatvec)
--       ( void *x, PRIMME_INT *ldx, void *y, PRIMME_INT *ldy, int *blockSize,
--         struct primme_params *primme, int *ierr);
-- type CmatrixMatvec = Ptr () -> Ptr PrimmeInt -> Ptr () -> Ptr PrimmeInt -> Ptr CInt -> Ptr Cprimme_params -> Ptr CInt

foo :: Int
foo = 123
