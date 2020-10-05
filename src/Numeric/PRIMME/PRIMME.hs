module Numeric.PRIMME.PRIMME (bar) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Vector.Storable
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)
import Numeric.PRIMME.Internal

-- void (*matrixMatvec)
--       ( void *x, PRIMME_INT *ldx, void *y, PRIMME_INT *ldy, int *blockSize,
--         struct primme_params *primme, int *ierr);
type CmatrixMatvec = Ptr () -> Ptr PrimmeInt -> Ptr () -> Ptr PrimmeInt -> Ptr CInt -> Ptr Cprimme_params -> Ptr CInt

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

withMatVec :: PrimmeDatatype a => MatVecType a -> (CmatrixMatvec -> IO b) -> IO b
withMatVec = undefined

foo :: Int
foo = 123
