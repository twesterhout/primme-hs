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

    -- * Choosing what to compute

    -- | Having defined a 'PrimmeOperator' we now need to tell PRIMME what to
    -- compute. This is done by constructing a 'PrimmeOptions' object.
    PrimmeOptions (..),
    primmeDefaults,
    finalizeOptions,
    PrimmeTarget (..),

    -- * Diagonalizing
    eigh,

    -- * Logging
    PrimmeMonitor (..),
    PrimmeInfo (..),
    PrimmeEventInfo (..),
    PrimmeStats (..),
    primmePrettyInfo,

    -- * Dense Matrices

    -- | The most trivial example of an "operator" is of course a square dense
    -- matrix. We thus provide a function which constructs an operator from a
    -- matrix.
    primmeFromDense,

    -- * Misc
    getPrimmeVersion,
    PrimmeException (..),
    BlasDatatype (blasTag),
    BlasRealPart,
    BlasDatatypeTag (..),
  )
where

import Control.Exception.Safe (throw)
import Control.Monad (unless)
import Control.Monad.ST (RealWorld)
import Data.Complex
import Data.Proxy
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Numeric.PRIMME.Context
import Numeric.PRIMME.Dense
import Numeric.PRIMME.Monitor
import Numeric.PRIMME.Options
import Numeric.PRIMME.Types

C.context (C.baseCtx <> primmeCtx)
C.include "<primme.h>"

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
  status <-
    MV.unsafeWith evals $ \evalsPtr ->
      MV.unsafeWith evecs $ \evecsPtr ->
        MV.unsafeWith rnorms $ \rnormsPtr ->
          withOptions options matrix $ \optionsPtr ->
            cPrimme evalsPtr evecsPtr rnormsPtr optionsPtr
  unless (status == 0) $ throwPrimmeError status
  evals' <- V.unsafeFreeze evals
  evecs' <- Block (dim, numEvals) dim <$> V.unsafeFreeze evecs
  rnorms' <- V.unsafeFreeze rnorms
  return (evals', evecs', rnorms')

throwPrimmeError :: CInt -> IO a
throwPrimmeError c = case (- c) of
  0 -> error "no error"
  3 -> throw PrimmeMaximumIterationsReached
  40 -> throw PrimmeLapackFailure
  41 -> throw PrimmeUserFailure
  _ -> throw $ PrimmeOtherFailure c

foreign import ccall "sprimme"
  sprimme :: Ptr Float -> Ptr Float -> Ptr Float -> Ptr Cprimme_params -> IO CInt

foreign import ccall "dprimme"
  dprimme :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Cprimme_params -> IO CInt

foreign import ccall "cprimme"
  cprimme :: Ptr Float -> Ptr (Complex Float) -> Ptr Float -> Ptr Cprimme_params -> IO CInt

foreign import ccall "zprimme"
  zprimme :: Ptr Double -> Ptr (Complex Double) -> Ptr Double -> Ptr Cprimme_params -> IO CInt

cPrimme ::
  forall a.
  (BlasDatatype a) =>
  Ptr (BlasRealPart a) ->
  Ptr a ->
  Ptr (BlasRealPart a) ->
  Ptr Cprimme_params ->
  IO CInt
cPrimme = case blasTag (Proxy :: Proxy a) of
  FloatTag -> sprimme
  DoubleTag -> dprimme
  ComplexFloatTag -> cprimme
  ComplexDoubleTag -> zprimme

getPrimmeVersion :: (Int, Int)
getPrimmeVersion =
  ( fromIntegral [CU.pure| int { PRIMME_VERSION_MAJOR } |],
    fromIntegral [CU.pure| int { PRIMME_VERSION_MINOR } |]
  )
