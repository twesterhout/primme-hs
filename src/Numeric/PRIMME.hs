-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- High-level wrapper around PRIMME C library
module Numeric.PRIMME
  ( -- * Defining the matrix

    -- | One of the great things about PRIMME library is that it works with
    -- block matrix-vector products (i.e. matrix-matrix products). Following the
    -- example of @vector@ library, we define two types for mutable and
    -- immutable dense blocks.
    MBlock (..),
    Block (..),
    -- | Now we are can define the "operator" we want to diagonalize. Since this
    -- library is meant to be used with rather large matrices, it might very
    -- well be the case that the matrix does not fit into the memory of your
    -- computer (even in sparse format such as CSR).  Sometimes, however, we can
    -- define the "operator" implicitly, namely, by defining its action on a
    -- vector (or in case of PRIMME on a block of vectors). This is done by
    -- 'PrimmeOperator' type.
    PrimmeOperator (..),
    -- | The most trivial example of an "operator" is of course a square dense
    -- matrix. We thus provide a function which constructs an operator from a
    -- matrix.
    fromDense,

    -- * Choosing what to compute

    -- | Having defined a 'PrimmeOperator' we now need to tell PRIMME what to
    -- compute. This is done by constructing a 'PrimmeOptions' object
    PrimmeOptions (..),
    PrimmeTarget (..),

    -- * Diagonalizing
    eigh,
  )
where

import Control.Exception (Exception, SomeException, bracket, catch, throw)
import Control.Monad (when)
import Control.Monad.ST (RealWorld)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable
import Numeric.PRIMME.Internal

-- | Matrix-vector product
-- type MatVecType a =
--   -- | Input vector @x@
--   Vector a ->
--   -- | Output vector @y@ where @Ax@ is written to
--   MVector RealWorld a ->
--   -- | Action is performed in 'IO' monad since it typically involves FFI
--   IO ()
data PrimmeException = PrimmeException !Text
  deriving (Show)

instance Exception PrimmeException

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
  deriving (Show)

-- | Matrix-matrix product which defines an operator @A@.
type PrimmeOperator a =
  -- | Matrix @x@ to which we apply @A@
  Block a ->
  -- | Where to store @Ax@
  MBlock RealWorld a ->
  -- | Procedure is run in 'IO' since it will typically involve FFI calls.
  IO ()

-- | Treat a dense symmetric or Hermitian matrix as a operator. Internally we
-- call @?symm@ or @?hemm@ BLAS functions so it is really important that the
-- matrix is indeed Hermitian.
fromDense ::
  PrimmeDatatype a =>
  -- | Dimension @dim@ of the matrix (i.e. number of rows or number of columns)
  Int ->
  -- | Stride along the second dimension (set it equal to @dim@ if your matrix
  -- is contiguous in memory).
  Int ->
  -- | Matrix elements in column-major order
  Vector a ->
  -- | Hermitian linear operator
  PrimmeOperator a
fromDense dim stride matrix
  | stride * dim == V.length matrix = \b c -> blockHemm 1 a b 0 c
  | otherwise =
    throw . PrimmeException . T.pack $
      "dimension mismatch: expected " <> show stride <> " * " <> show dim
        <> " elements, but the buffer has length "
        <> show (V.length matrix)
  where
    a = Block (dim, dim) stride matrix

blockHemm :: PrimmeDatatype a => a -> Block a -> Block a -> a -> MBlock RealWorld a -> IO ()
blockHemm α (Block (m, n) aStride a) (Block (n', k) bStride b) β (MBlock (m', k') cStride c) = do
  when (m /= n || m /= m' || n /= n' || k /= k') . error $
    "dimension mismatch: " <> show (m, n) <> " x " <> show (n', k) <> " = " <> show (m', k')
  hemm m' k' α a aStride b bStride β c cStride

-- | Which eigenpairs to target.
data PrimmeTarget
  = -- | Target smallest algebraic eigenvalues
    PrimmeSmallest
  | -- | Target largest algebraic eigenvalues
    PrimmeLargest
  deriving (Read, Show, Eq)

toCprimme_target :: PrimmeTarget -> Cprimme_target
toCprimme_target PrimmeSmallest = Cprimme_smallest
toCprimme_target PrimmeLargest = Cprimme_largest

-- | Specify some more information about the eigenvalue problem at hand.
--
-- /Warning:/ it is of utmost importance to use the correct dimension! If your
-- 'PrimmeOperator' has a different dimension than specified here, stuff will
-- break with segmentation faults...
data PrimmeOptions = PrimmeOptions
  { -- | Dimension of the operator. Please, set it carefully!
    pDim :: Int,
    -- | Number of eigenpairs to compute
    pNumEvals :: Int,
    -- | Which eigenpairs to target
    pTarget :: PrimmeTarget,
    -- | Tolerance
    pEps :: Double
  }

withOptions :: PrimmeDatatype a => PrimmeOptions -> PrimmeOperator a -> (Cprimme_params -> IO b) -> IO b
withOptions opts apply func = bracket acquire release worker
  where
    acquire = do
      p <- primme_params_create
      when (p == nullPtr) $ error "failed to allocate primme_params struct"
      return p
    release p = do
      c <- primme_params_destroy p
      when (c /= 0) $ error "failed to destroy primme_params struct"
    worker p = do
      primme_set_dim p (pDim opts)
      primme_set_num_evals p (pNumEvals opts)
      primme_set_target p (toCprimme_target . pTarget $ opts)
      primme_set_eps p (pEps opts)
      primme_set_print_level p 5
      c <- primme_set_method PRIMME_DEFAULT_MIN_MATVECS p
      when (c /= 0) $ error "failed to set method"
      withOperator apply $ \matvecPtr ->
        primme_set_matvec p matvecPtr >> func p

withOperator :: PrimmeDatatype a => PrimmeOperator a -> (FunPtr CmatrixMatvec -> IO b) -> IO b
withOperator !f = withCmatrixMatvec cWrapper
  where
    cWrapper !xPtr !xStridePtr !yPtr !yStridePtr !blockSizePtr !params !errPtr = do
      n <- primme_get_dim params
      blockSize <- fromIntegral <$> peek blockSizePtr
      xStride <- fromIntegral <$> peek xStridePtr
      yStride <- fromIntegral <$> peek yStridePtr
      x <-
        Block (n, blockSize) xStride
          <$> flip V.unsafeFromForeignPtr0 (xStride * blockSize)
          <$> newForeignPtr_ (castPtr xPtr)
      y <-
        MBlock (n, blockSize) yStride
          <$> flip MV.unsafeFromForeignPtr0 (yStride * blockSize)
          <$> newForeignPtr_ (castPtr yPtr)
      (f x y >> poke errPtr 0) `catch` (\(_ :: SomeException) -> poke errPtr (-1))

-- | Diagonalize the operator to find the first few eigenpairs.
--
-- For every eigenpair a tuple @(eigenvalue, eigenvector, residual norm)@ is
-- returned.
eigh :: forall a. PrimmeDatatype a => PrimmeOptions -> PrimmeOperator a -> IO [(RealPart a, Vector a, RealPart a)]
eigh options matrix = do
  let dim = pDim options
      numEvals = pNumEvals options
  (evals :: MVector RealWorld (RealPart a)) <- MV.new numEvals
  (evecs :: MVector RealWorld a) <- MV.new (dim * numEvals)
  (rnorms :: MVector RealWorld (RealPart a)) <- MV.new numEvals
  status <-
    MV.unsafeWith evals $ \evalsPtr ->
      MV.unsafeWith evecs $ \evecsPtr ->
        MV.unsafeWith rnorms $ \rnormsPtr ->
          withOptions options matrix $ \optionsPtr ->
            cPrimme evalsPtr evecsPtr rnormsPtr optionsPtr
  when (status /= 0) . throw . PrimmeException . T.pack $
    "?primme failed with error code " <> show status
  evals' <- V.toList <$> V.unsafeFreeze evals
  evecs' <- zipWith (\i xs -> V.slice (i * dim) dim xs) [0 .. pNumEvals options - 1] . repeat <$> V.unsafeFreeze evecs
  rnorms' <- V.toList <$> V.unsafeFreeze rnorms
  return $ zip3 evals' evecs' rnorms'
