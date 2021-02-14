{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.PRIMME.Types
  ( PrimmeException (..),
    BlasDatatype (..),
    BlasRealPart,
    BlasDatatypeTag (..),
    MBlock (..),
    Block (..),
    PrimmeOperator,
    PrimmeTarget (..),
    PrimmeMethod (..),
    PrimmeInfo (..),
    PrimmeStats (..),
    PrimmeEventInfo (..),
    PrimmeMonitor (..),
    PrimmeOptions (..),
    PrimmeInt,
    Cprimme_params,
    CmatrixMatvec,
    CmonitorFun,
  )
where

import Control.Exception.Safe
import Control.Monad.ST (RealWorld)
import Data.Complex
import Data.Int (Int32, Int64)
import Data.Kind
import Data.Text
import Data.Vector.Storable (MVector, Vector)
import Foreign.C.Types (CChar, CDouble, CInt, CLong)
import Foreign.Ptr (Ptr)
import Foreign.Storable

#if !defined(PRIMME_INT_SIZE) || PRIMME_INT_SIZE == 64
type PrimmeInt = CLong -- Int64
#elif PRIMME_INT_SIZE == 0
type PrimmeInt = CInt
#elif PRIMME_INT_SIZE == 32
type PrimmeInt = Int32
#else
#error "invalid value of PRIMME_INT_SIZE"
#endif

data Cprimme_params

-- | Type of blocked matrix-vector product accepted by PRIMME C library.
-- @
-- void (*matrixMatvec)
--       ( void *x, PRIMME_INT *ldx, void *y, PRIMME_INT *ldy, int *blockSize,
--         struct primme_params *primme, int *ierr);
-- @
type CmatrixMatvec = Ptr () -> Ptr PrimmeInt -> Ptr () -> Ptr PrimmeInt -> Ptr CInt -> Ptr Cprimme_params -> Ptr CInt -> IO ()

-- | Type of logging function.
-- @
-- void (*monitorFun)(void *basisEvals, int *basisSize, int *basisFlags, int *iblock,
--                    int *blockSize, void *basisNorms, int *numConverged, void *lockedEvals,
--                    int *numLocked, int *lockedFlags, void *lockedNorms, int *inner_its,
--                    void *LSRes, const char *msg, double *time, primme_event *event,
--                    struct primme_params *primme, int *ierr)
-- @
type CmonitorFun =
  Ptr () ->
  Ptr CInt ->
  Ptr CInt ->
  Ptr CInt ->
  Ptr CInt ->
  Ptr () ->
  Ptr CInt ->
  Ptr () ->
  Ptr CInt ->
  Ptr CInt ->
  Ptr () ->
  Ptr CInt ->
  Ptr () ->
  Ptr CChar ->
  Ptr CDouble ->
  Ptr CInt ->
  Ptr Cprimme_params ->
  Ptr CInt ->
  IO ()

-- | Exceptions thrown by this package.
data PrimmeException
  = PrimmeMaximumIterationsReached
  | PrimmeUserFailure
  | PrimmeLapackFailure
  | PrimmeOtherFailure {-# UNPACK #-} !CInt
  deriving stock (Show)

instance Exception PrimmeException

-- | A GADT which allows to dispatch between BLAS types at runtime.
data BlasDatatypeTag :: (Type -> Type) where
  FloatTag :: BlasDatatypeTag Float
  DoubleTag :: BlasDatatypeTag Double
  ComplexFloatTag :: BlasDatatypeTag (Complex Float)
  ComplexDoubleTag :: BlasDatatypeTag (Complex Double)

type family BlasRealPart a where
  BlasRealPart Float = Float
  BlasRealPart Double = Double
  BlasRealPart (Complex Float) = Float
  BlasRealPart (Complex Double) = Double

-- | BLAS datatype.
class
  ( Storable a,
    Storable (BlasRealPart a),
    Floating a,
    Floating (BlasRealPart a),
    Show a,
    Show (BlasRealPart a)
  ) =>
  BlasDatatype a
  where
  blasTag :: proxy a -> BlasDatatypeTag a

instance BlasDatatype Float where blasTag _ = FloatTag

instance BlasDatatype Double where blasTag _ = DoubleTag

instance BlasDatatype (Complex Float) where blasTag _ = ComplexFloatTag

instance BlasDatatype (Complex Double) where blasTag _ = ComplexDoubleTag

-- | Mutable dense matrix in column-major order.
data MBlock s a = MBlock
  { -- | Shape of the matrix as @(number of rows, number of columns)@
    mutableBlockShape :: {-# UNPACK #-} !(Int, Int),
    -- | Stride along the second dimension (typically called @ldim@ or "leading
    -- dimension" in BLAS-like libraries). Since the matrix is in column-major
    -- order, stride along the first dimension is always 1.
    mutableBlockStride :: {-# UNPACK #-} !Int,
    -- | The actual data buffer of length @number of columns * mutableBlockStride@
    mutableBlockData :: {-# UNPACK #-} !(MVector s a)
  }

-- | Dense matrix in column-major order.
data Block a = Block
  { -- | Shape of the matrix as @(number of rows, number of columns)@
    blockShape :: {-# UNPACK #-} !(Int, Int),
    -- | Stride along the second dimension (typically called @ldim@ or "leading
    -- dimension" in BLAS-like libraries). Since the matrix is in column-major
    -- order, stride along the first dimension is always 1.
    blockStride :: {-# UNPACK #-} !Int,
    -- | The actual data buffer of length @#columns * blockStride@
    blockData :: {-# UNPACK #-} !(Vector a)
  }
  deriving stock (Show)

-- | Matrix-matrix product which defines an operator @A@.
type PrimmeOperator a =
  -- | Matrix @x@ to which we apply @A@
  Block a ->
  -- | Where to store @Ax@
  MBlock RealWorld a ->
  -- | Procedure is run in 'IO' since it will typically involve FFI calls.
  IO ()

-- | Which eigenpairs to target.
--
-- /Note:/ PRIMME C library also supports computing non-extremal eigenvalues.
-- Wrappers for it may be added in the future.
data PrimmeTarget
  = -- | Target smallest algebraic eigenvalues
    PrimmeSmallest
  | -- | Target largest algebraic eigenvalues
    PrimmeLargest
  deriving stock (Read, Show, Eq)

-- | Which method to use.
data PrimmeMethod
  = PrimmeDynamic
  | -- | GD+k
    PrimmeGDOlsenPlusK
  | -- | JDQMR
    PrimmeJDQMRETol
  deriving stock (Read, Show, Eq)

newtype PrimmeMonitor = PrimmeMonitor
  { unPrimmeMonitor :: forall a. BlasDatatype a => PrimmeInfo a -> IO Bool
  }

data PrimmeInfo a = PrimmeInfo (PrimmeEventInfo a) PrimmeStats

deriving stock instance BlasDatatype a => Show (PrimmeInfo a)

data PrimmeStats = PrimmeStats
  { pStatsNumOuterIterations :: !Int,
    pStatsNumRestarts :: !Int,
    pStatsNumMatvecs :: !Int,
    pStatsTimeMatvec :: !Double,
    pStatsTimeOrtho :: !Double,
    pStatsTimeDense :: !Double
  }
  deriving stock (Show)

data PrimmeEventInfo a
  = PrimmeOuterInfo !(Vector (BlasRealPart a)) !(Vector (BlasRealPart a))
  | PrimmeInnerInfo
  | PrimmeLockedInfo
  | PrimmeConvergedInfo !Int !(BlasRealPart a) !(BlasRealPart a)
  | PrimmeMessageInfo !Text

deriving stock instance BlasDatatype a => Show (PrimmeEventInfo a)

-- | Specify some more information about the eigenvalue problem at hand.
--
-- /Warning:/ it is of utmost importance to use the correct dimension! If your
-- 'PrimmeOperator' has a different dimension than specified here, stuff will
-- break with segmentation faults...
data PrimmeOptions = PrimmeOptions
  { -- | Dimension of the operator. Please, set it carefully!
    pDim :: !Int,
    -- | Number of eigenpairs to compute
    pNumEvals :: !Int,
    -- | Which eigenpairs to target
    pTarget :: !PrimmeTarget,
    -- | Which algorithm to use
    pMethod :: !PrimmeMethod,
    -- | Tolerance
    pEps :: !Double,
    -- | Maximal basis size. [See also](http://www.cs.wm.edu/~andreas/software/doc/appendix.html#c.primme_params.maxBasisSize)
    pMaxBasisSize :: !Int,
    -- | Maximum number of Ritz vectors kept after restarting the basis.
    -- [See also](http://www.cs.wm.edu/~andreas/software/doc/appendix.html#c.primme_params.minRestartSize)
    pMinRestartSize :: !Int,
    -- | Maximal block size. [See also](http://www.cs.wm.edu/~andreas/software/doc/appendix.html#c.primme_params.maxBlockSize)
    pMaxBlockSize :: !Int,
    -- | Logging
    pLogAction :: !(Either Int PrimmeMonitor)
  }
