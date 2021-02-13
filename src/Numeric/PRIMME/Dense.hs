-- |
-- Copyright: (c) 2020-2021 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module Numeric.PRIMME.Dense
  ( -- * Treat dense matrices as linear operators
    primmeFromDense,
  )
where

import Control.Monad.ST (RealWorld)
import Data.Complex
import Data.Proxy
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types (CChar, CInt)
import Foreign.Marshal.Utils
import Foreign.Ptr (Ptr)
import Numeric.PRIMME.Types

type BlasInt = CInt

type BlasHemmType a =
  -- | SIDE
  Ptr CChar ->
  -- | UPLO
  Ptr CChar ->
  -- | M
  Ptr BlasInt ->
  -- | N
  Ptr BlasInt ->
  -- | ALPHA
  Ptr a ->
  -- | A
  Ptr a ->
  -- | LDA
  Ptr BlasInt ->
  -- | B
  Ptr a ->
  -- | LDB
  Ptr BlasInt ->
  -- | BETA
  Ptr a ->
  -- | C
  Ptr a ->
  -- | LDC
  Ptr BlasInt ->
  IO ()

foreign import ccall unsafe "ssymm_" ssymm_ :: BlasHemmType Float

foreign import ccall unsafe "dsymm_" dsymm_ :: BlasHemmType Double

foreign import ccall unsafe "chemm_" chemm_ :: BlasHemmType (Complex Float)

foreign import ccall unsafe "zhemm_" zhemm_ :: BlasHemmType (Complex Double)

hemm :: BlasDatatype a => Int -> Int -> a -> Vector a -> Int -> Vector a -> Int -> a -> MVector RealWorld a -> Int -> IO ()
hemm m n α a aStride b bStride β c cStride = do
  with (fromIntegral (fromEnum 'L') :: CChar) $ \side' ->
    with (fromIntegral (fromEnum 'U') :: CChar) $ \uplo' -> do
      with (fromIntegral m) $ \m' ->
        with (fromIntegral n) $ \n' ->
          with (fromIntegral aStride) $ \aStride' ->
            with (fromIntegral bStride) $ \bStride' ->
              with (fromIntegral cStride) $ \cStride' ->
                with α $ \α' ->
                  with β $ \β' ->
                    V.unsafeWith a $ \aPtr ->
                      V.unsafeWith b $ \bPtr ->
                        MV.unsafeWith c $ \cPtr ->
                          hemm' side' uplo' m' n' α' aPtr aStride' bPtr bStride' β' cPtr cStride'
  where
    hemm' :: forall b. BlasDatatype b => BlasHemmType b
    hemm' = case blasTag (Proxy :: Proxy b) of
      FloatTag -> ssymm_
      DoubleTag -> dsymm_
      ComplexFloatTag -> chemm_
      ComplexDoubleTag -> zhemm_

blockHemm :: BlasDatatype a => a -> Block a -> Block a -> a -> MBlock RealWorld a -> IO ()
blockHemm α (Block (m, n) aStride a) (Block (n', k) bStride b) β (MBlock (m', k') cStride c)
  | m /= n || m /= m' || n /= n' || k /= k' =
    error $
      "dimension mismatch: " <> show (m, n) <> " x " <> show (n', k) <> " = " <> show (m', k')
  | otherwise = hemm m' k' α a aStride b bStride β c cStride

-- | Treat a dense symmetric or Hermitian matrix as a operator. Internally we
-- call
-- [@?symm@](https://www.netlib.org/lapack/explore-html/db/dc9/group__single__blas__level3_ga8e8391a9873114d97e2b63e39fe83b2e.html#ga8e8391a9873114d97e2b63e39fe83b2e)
-- or
-- [@?hemm@](https://www.netlib.org/lapack/explore-html/db/def/group__complex__blas__level3_gad2d1853a142397404eae974b6574ece3.html#gad2d1853a142397404eae974b6574ece3)
-- BLAS functions so it is really important that the matrix is indeed Hermitian.
primmeFromDense ::
  BlasDatatype a =>
  -- | Matrix in column-major order
  Block a ->
  -- | Hermitian linear operator
  PrimmeOperator a
primmeFromDense a
  | isHermitian a = \b c -> blockHemm 1 a b 0 c
  | otherwise = error "expected a Hermitian matrix"

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
    -- Return complex conjugate of a number.
    !conj = case blasTag (Proxy :: Proxy a) of
      FloatTag -> id
      DoubleTag -> id
      ComplexFloatTag -> Data.Complex.conjugate
      ComplexDoubleTag -> Data.Complex.conjugate
