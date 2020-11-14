-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- High-level wrapper around [PRIMME C
-- library](https://github.com/primme/primme). /Quote from README/:
--
-- @
--   PRIMME, pronounced as prime, is a high-performance library for computing a few
--   eigenvalues/eigenvectors, and singular values/vectors. PRIMME is especially
--   optimized for large, difficult problems. Real symmetric and complex Hermitian
--   problems, standard @A x = λ x@ and generalized @A x = λ B x@, are supported.
--   Besides, standard eigenvalue problems with a normal matrix are supported. It
--   can find largest, smallest, or interior singular/eigenvalues, and can use
--   preconditioning to accelerate convergence.
-- @
module Numeric.PRIMME
  ( -- * Defining the matrix

    -- | One of the great things about PRIMME library is that it works with
    -- block matrix-vector products (i.e. matrix-matrix products). Following the
    -- example of [vector](https://hackage.haskell.org/package/vector) library,
    -- we define two types for mutable and immutable dense blocks.
    MBlock (..),
    Block (..),
    -- | Now we are can define the "operator" we want to diagonalize. Since this
    -- library is meant to be used with rather large matrices, it might very
    -- well be the case that the matrix does not fit into the memory of your
    -- computer (even in sparse format such as CSR). Sometimes, however, we can
    -- define the "operator" implicitly, namely, by defining its action on a
    -- vector (or in case of PRIMME on a block of vectors). This is done with
    -- 'PrimmeOperator' type.
    PrimmeOperator,
    -- | The most trivial example of an "operator" is of course a square dense
    -- matrix. We thus provide a function which constructs an operator from a
    -- matrix.
    fromDense,

    -- * Choosing what to compute

    -- | Having defined a 'PrimmeOperator' we now need to tell PRIMME what to
    -- compute. This is done by constructing a 'PrimmeOptions' object.
    PrimmeOptions (..),
    defaultOptions,
    PrimmeTarget (..),

    -- * Diagonalizing
    eigh,

    -- * Misc
    PrimmeException (..),
    BlasDatatype (blasTag),
    BlasRealPart,
    BlasDatatypeTag (..),
  )
where

import Control.Exception.Safe (Exception, MonadThrow, SomeException, bracket, catch, impureThrow, throw)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Control.Monad.ST (RealWorld)
import qualified Data.Complex
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr
import Foreign.Ptr (FunPtr, castPtr, nullPtr)
import Foreign.Storable
import Numeric.PRIMME.Internal

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
-- call
-- [@?symm@](https://www.netlib.org/lapack/explore-html/db/dc9/group__single__blas__level3_ga8e8391a9873114d97e2b63e39fe83b2e.html#ga8e8391a9873114d97e2b63e39fe83b2e)
-- or
-- [@?hemm@](https://www.netlib.org/lapack/explore-html/db/def/group__complex__blas__level3_gad2d1853a142397404eae974b6574ece3.html#gad2d1853a142397404eae974b6574ece3)
-- BLAS functions so it is really important that the matrix is indeed Hermitian.
fromDense ::
  BlasDatatype a =>
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
  | stride * dim /= V.length matrix =
    impureThrow . PrimmeException . T.pack $
      "dimension mismatch: expected " <> show stride <> " * " <> show dim
        <> " elements, but the buffer has length "
        <> show (V.length matrix)
  | not (isHermitian a) = impureThrow . PrimmeException $ "expected a Hermitian matrix"
  | otherwise = \b c -> blockHemm 1 a b 0 c
  where
    a = Block (dim, dim) stride matrix

blockHemm :: BlasDatatype a => a -> Block a -> Block a -> a -> MBlock RealWorld a -> IO ()
blockHemm α (Block (m, n) aStride a) (Block (n', k) bStride b) β (MBlock (m', k') cStride c) = do
  when (m /= n || m /= m' || n /= n' || k /= k') . error $
    "dimension mismatch: " <> show (m, n) <> " x " <> show (n', k) <> " = " <> show (m', k')
  hemm m' k' α a aStride b bStride β c cStride

-- | Determine whether a matrix is Hermitian (or symmetric when @a@ is real).
--
-- /Note:/ this function returns 'False' for non-square matrices. No exceptions
-- are thrown.
isHermitian :: forall a. BlasDatatype a => Block a -> Bool
isHermitian (Block (n, n') stride v)
  | n == n' =
    -- Iterate in column-major order here and over the lower part of the matrix
    loop 0 (n - 1) $ \j ->
      loop (j + 1) n $ \i ->
        check i j
  | otherwise = False
  where
    access !i !j = v V.! (i + stride * j)
    check !i !j = access i j `unsafeEq` conj (access j i)
    loop !i !high f
      | i < high =
        if f i
          then loop (i + 1) high f
          else False
      | otherwise = True
    -- Yes, we really want exact binary comparison of floating point types
    unsafeEq :: a -> a -> Bool
    unsafeEq = case blasTag (Proxy :: Proxy a) of
      FloatTag -> (==)
      DoubleTag -> (==)
      ComplexFloatTag -> (==)
      ComplexDoubleTag -> (==)

-- | Return complex conjugate of a number. For real numbers this is just 'id'.
conj :: forall a. BlasDatatype a => a -> a
conj = case blasTag (Proxy :: Proxy a) of
  FloatTag -> id
  DoubleTag -> id
  ComplexFloatTag -> Data.Complex.conjugate
  ComplexDoubleTag -> Data.Complex.conjugate
{-# INLINE conj #-}

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
    pEps :: Double,
    -- |
    pMaxBasisSize :: Int,
    -- |
    pMaxBlockSize :: Int
  }

defaultOptions :: PrimmeOptions
defaultOptions =
  PrimmeOptions
    { pDim = -1,
      pNumEvals = 1,
      pTarget = PrimmeSmallest,
      pEps = 0.0,
      pMaxBasisSize = -1,
      pMaxBlockSize = -1
    }

initOptions :: (MonadIO m, MonadThrow m) => PrimmeOptions -> Cprimme_params -> m ()
initOptions options c_options = do
  when (pDim options == -1) . throw . PrimmeException $
    "pDim in PrimmeOptions is equal to the default value -1; please, set it "
      <> "to the dimension of your operator prior to calling eigh"
  when (pDim options <= 0) . throw . PrimmeException . T.pack $
    "pDim in PrimmeOptions is invalid: " <> show (pDim options) <> "; expected a positive integer"
  liftIO $ primme_set_dim c_options (pDim options)
  --
  when (pNumEvals options <= 0) . throw . PrimmeException . T.pack $
    "pNumEvals in PrimmeOptions is invalid: " <> show (pNumEvals options) <> "; expected a positive integer"
  when (pNumEvals options > pDim options) . throw . PrimmeException . T.pack $
    "pNumEvals in PrimmeOptions is invalid: " <> show (pNumEvals options)
      <> "; number of eigenpairs cannot exceed the dimension of the operator"
  liftIO $ primme_set_num_evals c_options (pNumEvals options)
  --
  liftIO $ primme_set_target c_options (toCprimme_target . pTarget $ options)
  --
  when (pEps options < 0.0) . throw . PrimmeException . T.pack $
    "pEps in PrimmeOptions is invalid: " <> show (pEps options) <> "; expected a non-negative number"
  liftIO $ primme_set_eps c_options (pEps options)
  --
  liftIO $ primme_set_print_level c_options 5
  --
  unless (pMaxBasisSize options == -1) $ do
    when (pMaxBasisSize options < 2) . throw . PrimmeException . T.pack $
      "pMaxBasisSize in PrimmeOptions is invalid: " <> show (pMaxBasisSize options)
        <> "; basis size must be at least 2"
    liftIO $ primme_set_max_basis_size c_options (pMaxBasisSize options)
  --
  unless (pMaxBlockSize options == -1) $ do
    when (pMaxBlockSize options <= 0) . throw . PrimmeException . T.pack $
      "pMaxBlockSize in PrimmeOptions is invalid: " <> show (pMaxBlockSize options)
        <> "; expected a positive integer"
    liftIO $ primme_set_max_block_size c_options (pMaxBlockSize options)
  --
  liftIO $ primme_set_method Cprimme_DEFAULT_MIN_MATVECS c_options

withOptions :: BlasDatatype a => PrimmeOptions -> PrimmeOperator a -> (Cprimme_params -> IO b) -> IO b
withOptions opts apply func = bracket primme_params_create primme_params_destroy $ \p -> do
  initOptions opts p
  withOperator apply $ \matvecPtr ->
    primme_set_matvec p matvecPtr >> func p

withOperator :: BlasDatatype a => PrimmeOperator a -> (FunPtr CmatrixMatvec -> IO b) -> IO b
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
eigh ::
  forall a.
  BlasDatatype a =>
  PrimmeOptions ->
  PrimmeOperator a ->
  IO (Vector (BlasRealPart a), Block a, Vector (BlasRealPart a))
eigh options matrix = do
  let dim = pDim options
      numEvals = pNumEvals options
  (evals :: MVector RealWorld (BlasRealPart a)) <- MV.new numEvals
  (evecs :: MVector RealWorld a) <- MV.new (dim * numEvals)
  (rnorms :: MVector RealWorld (BlasRealPart a)) <- MV.new numEvals
  _ <- (checkError =<<) $
    MV.unsafeWith evals $ \evalsPtr ->
      MV.unsafeWith evecs $ \evecsPtr ->
        MV.unsafeWith rnorms $ \rnormsPtr ->
          withOptions options matrix $ \optionsPtr ->
            cPrimme evalsPtr evecsPtr rnormsPtr optionsPtr
  evals' <- V.unsafeFreeze evals
  evecs' <- Block (dim, numEvals) dim <$> V.unsafeFreeze evecs
  rnorms' <- V.unsafeFreeze rnorms
  return (evals', evecs', rnorms')
