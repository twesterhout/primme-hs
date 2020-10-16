module Numeric.PRIMME.Internal where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.ST (RealWorld)
import Data.Coerce
import Data.Complex
import Data.Proxy
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Storable
import Foreign.C.Types (CChar, CInt, CFloat, CDouble, CLong)
import Foreign.Ptr (Ptr, castPtr, nullPtr, FunPtr, freeHaskellFunPtr)
import Foreign.Marshal.Utils

#include <primme.h>
#include "wrapper.h"


{#typedef PRIMME_INT Cprimme_int#}

{#enum primme_target as Cprimme_target
  { primme_smallest as Cprimme_smallest
  , primme_largest as Cprimme_largest
  , primme_closest_geq as Cprimme_closest_geq
  , primme_closest_leq as Cprimme_closest_leq
  , primme_closest_abs as Cprimme_closest_abs
  , primme_largest_abs as Cprimme_largest_abs } #}

{#enum primme_op_datatype as Cprimme_op_datatype
  { primme_op_default as Cprimme_op_default
  , primme_op_half as Cprimme_op_half
  , primme_op_float as Cprimme_op_float
  , primme_op_double as Cprimme_op_double
  , primme_op_quad as Cprimme_op_quad
  , primme_op_int as Cprimme_op_int } #}

{#enum primme_preset_method as Cprimme_preset_method
  { } #}
-- { PRIMME_DEFAULT_METHOD
-- , PRIMME_DYNAMIC
-- , PRIMME_DEFAULT_MIN_TIME
-- , PRIMME_DEFAULT_MIN_MATVECS
-- , PRIMME_Arnoldi
-- , PRIMME_GD
-- , PRIMME_GD_plusK
-- , PRIMME_GD_Olsen_plusK
-- , PRIMME_JD_Olsen_plusK
-- , PRIMME_RQI
-- , PRIMME_JDQR
-- , PRIMME_JDQMR
-- , PRIMME_JDQMR_ETol
-- , PRIMME_STEEPEST_DESCENT
-- , PRIMME_LOBPCG_OrthoBasis
-- , PRIMME_LOBPCG_OrthoBasis_Window }



{#pointer *primme_params as Cprimme_params#}

{#fun unsafe primme_params_create { } -> `Cprimme_params' #}
{#fun unsafe primme_params_destroy { `Cprimme_params' } -> `CInt' #}
{#fun unsafe primme_set_method { `Cprimme_preset_method', `Cprimme_params' } -> `CInt' #}
{#fun wrap_primme_display_params as primme_display_params { `Cprimme_params' } -> `()' #}

-- int dprimme(double *evals, double *evecs, double *resNorms, primme_params *primme)
{#fun sprimme { id `Ptr CFloat', id `Ptr CFloat', id `Ptr CFloat', `Cprimme_params' } -> `CInt' #}
{#fun dprimme { id `Ptr CDouble', id `Ptr CDouble', id `Ptr CDouble', `Cprimme_params' } -> `CInt' #}

primme_set_dim :: Cprimme_params -> Int -> IO ()
primme_set_dim p n
  | n <= 0 = error $ "invalid matrix dimension: " <> show n
  | otherwise = {#set primme_params.n#} p (fromIntegral n)

primme_get_dim :: Cprimme_params -> IO Int
primme_get_dim p = fromIntegral <$> {#get primme_params.n#} p

primme_set_num_evals :: Cprimme_params -> Int -> IO ()
primme_set_num_evals p n
  | n <= 0 = invalidArgument
  | otherwise = do
      dim <- primme_get_dim p
      when (n > dim) invalidArgument
      {#set primme_params.numEvals#} p (fromIntegral n)
  where invalidArgument = error $ "invalid number of eigenvalues: " <> show n

primme_set_target :: Cprimme_params -> Cprimme_target -> IO ()
primme_set_target p t = {#set primme_params.target#} p (fromIntegral . fromEnum $ t)

primme_set_eps :: Cprimme_params -> Double -> IO ()
primme_set_eps p eps = {#set primme_params.eps#} p (coerce eps)

bar :: IO ()
bar = do
  p <- primme_params_create
  when (p == nullPtr) $ error "primme_params_create failed"
  c <- primme_set_method PRIMME_DEFAULT_MIN_MATVECS p
  when (c < 0) $ error "primme_set_method failed"
  primme_display_params p
  primme_params_destroy p
  return ()

type PrimmeInt = CLong


-- void (*matrixMatvec)
--       ( void *x, PRIMME_INT *ldx, void *y, PRIMME_INT *ldy, int *blockSize,
--         struct primme_params *primme, int *ierr);
type CmatrixMatvec = Ptr () -> Ptr PrimmeInt -> Ptr () -> Ptr PrimmeInt -> Ptr CInt -> Cprimme_params -> Ptr CInt -> IO ()

foreign import ccall "wrapper"
    mkCmatrixMatvec :: CmatrixMatvec -> IO (FunPtr CmatrixMatvec)

withCmatrixMatvec :: CmatrixMatvec -> (FunPtr CmatrixMatvec -> IO a) -> IO a
withCmatrixMatvec f = bracket (mkCmatrixMatvec f) freeHaskellFunPtr

type family RealPart a where
  RealPart Float = Float
  RealPart CFloat = CFloat
  RealPart Double = Double
  RealPart CDouble = CDouble

type BlasInt = CInt

type BlasHemmType a
  = Ptr CChar -- ^ SIDE
 -> Ptr CChar -- ^ UPLO
 -> Ptr BlasInt -- ^ M
 -> Ptr BlasInt -- ^ N
 -> Ptr a -- ^ ALPHA
 -> Ptr a -- ^ A
 -> Ptr BlasInt -- ^ LDA
 -> Ptr a -- ^ B
 -> Ptr BlasInt -- ^ LDB
 -> Ptr a -- ^ BETA
 -> Ptr a -- ^ C
 -> Ptr BlasInt -- ^ LDC
 -> IO ()

foreign import ccall "ssymm_" ssymm_ :: BlasHemmType Float
foreign import ccall "dsymm_" dsymm_ :: BlasHemmType Double
foreign import ccall "chemm_" chemm_ :: BlasHemmType (Complex Float)
foreign import ccall "zhemm_" zhemm_ :: BlasHemmType (Complex Double)

class (Num a, Storable a, Storable (RealPart a)) => PrimmeDatatype a where
  cDatatype :: Proxy a -> Cprimme_op_datatype
  cPrimme :: Ptr (RealPart a) -> Ptr a -> Ptr (RealPart a) -> Cprimme_params -> IO CInt
  cHemm :: BlasHemmType a

instance PrimmeDatatype Float where
  cDatatype _ = Cprimme_op_float
  cPrimme evals evecs rnorms params = sprimme (castPtr evals) (castPtr evecs) (castPtr rnorms) params
  cHemm = ssymm_

instance PrimmeDatatype CFloat where
  cDatatype _ = Cprimme_op_float
  cPrimme = sprimme
  cHemm = coerce ssymm_

instance PrimmeDatatype Double where
  cDatatype _ = Cprimme_op_double
  cPrimme evals evecs rnorms params = dprimme (castPtr evals) (castPtr evecs) (castPtr rnorms) params
  cHemm = dsymm_

instance PrimmeDatatype CDouble where
  cDatatype _ = Cprimme_op_double
  cPrimme = dprimme
  cHemm = coerce dsymm_

hemm :: PrimmeDatatype a => Int -> Int -> a -> Vector a -> Int -> Vector a -> Int -> a -> MVector RealWorld a -> Int -> IO ()
hemm m n α a aStride b bStride β c cStride = do
  with (fromIntegral (fromEnum 'L') :: CChar) $ \side' ->
    with (fromIntegral (fromEnum 'U') :: CChar) $ \uplo' ->
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
                          cHemm side' uplo' m' n' α' aPtr aStride' bPtr bStride' β' cPtr cStride'

primme_set_matvec :: Cprimme_params -> FunPtr CmatrixMatvec -> IO ()
primme_set_matvec = {#set primme_params.matrixMatvec#}
