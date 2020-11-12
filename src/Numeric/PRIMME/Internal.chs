module Numeric.PRIMME.Internal
  ( -- PrimmeDatatype (..),
    BlasDatatype (..),
    BlasRealPart,
    BlasDatatypeTag (..),
    Cprimme_params,
    Cprimme_target (..),
    Cprimme_preset_method (..),
    CmatrixMatvec,
    withCmatrixMatvec,
    primme_get_dim,
    primme_set_dim,
    primme_set_num_evals,
    primme_set_eps,
    primme_set_target,
    primme_set_method,
    primme_set_matvec,
    primme_set_print_level,
    primme_params_create,
    primme_params_destroy,
    sprimme,
    dprimme,
    cPrimme,
    hemm,
  )
where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.ST (RealWorld)
import Data.Complex (Complex)
import Data.Coerce
import Data.Proxy
import Data.Kind
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Storable
import Foreign.C.Types (CChar, CInt, CFloat, CDouble, CLong)
import Foreign.Ptr (Ptr, castPtr, FunPtr, freeHaskellFunPtr)
import Foreign.Marshal.Utils

#include <primme.h>
#include "wrapper.h"


type PrimmeInt = {#type wrap_primme_int#}

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

{#pointer *primme_params as Cprimme_params#}

{#fun unsafe primme_params_create { } -> `Cprimme_params' #}
{#fun unsafe primme_params_destroy { `Cprimme_params' } -> `CInt' #}
{#fun unsafe primme_set_method { `Cprimme_preset_method', `Cprimme_params' } -> `CInt' #}
-- {#fun wrap_primme_display_params as primme_display_params { `Cprimme_params' } -> `()' #}

{#fun sprimme { castPtr' `Ptr Float', castPtr' `Ptr Float', castPtr' `Ptr Float', `Cprimme_params' } -> `CInt' id #}
{#fun dprimme { castPtr' `Ptr Double', castPtr' `Ptr Double', castPtr' `Ptr Double', `Cprimme_params' } -> `CInt' id #}
-- C2HS refuses to handle C99 complex types properly
foreign import ccall "cprimme"
  cprimme :: Ptr Float -> Ptr (Complex Float) -> Ptr Float -> Cprimme_params -> IO CInt

foreign import ccall "zprimme"
  zprimme :: Ptr Double -> Ptr (Complex Double) -> Ptr Double -> Cprimme_params -> IO CInt

primme_set_dim :: Cprimme_params -> Int -> IO ()
primme_set_dim p n
  | n <= 0 = error $ "invalid matrix dimension: " <> show n
  | otherwise = {#set primme_params.n#} p (fromIntegral n)

primme_get_dim :: Cprimme_params -> IO Int
primme_get_dim p = fromIntegral <$> {#get primme_params.n#} p

primme_set_print_level :: Cprimme_params -> Int -> IO ()
primme_set_print_level p n = {#set primme_params.printLevel#} p (fromIntegral n)

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

-- | Type of matrix-vector product accepted by PRIMME C library.
-- @
-- void (*matrixMatvec)
--       ( void *x, PRIMME_INT *ldx, void *y, PRIMME_INT *ldy, int *blockSize,
--         struct primme_params *primme, int *ierr);
-- @
type CmatrixMatvec = Ptr () -> Ptr PrimmeInt -> Ptr () -> Ptr PrimmeInt -> Ptr CInt -> Cprimme_params -> Ptr CInt -> IO ()

foreign import ccall "wrapper"
    mkCmatrixMatvec :: CmatrixMatvec -> IO (FunPtr CmatrixMatvec)

withCmatrixMatvec :: CmatrixMatvec -> (FunPtr CmatrixMatvec -> IO a) -> IO a
withCmatrixMatvec f = bracket (mkCmatrixMatvec f) freeHaskellFunPtr

primme_set_matvec :: Cprimme_params -> FunPtr CmatrixMatvec -> IO ()
primme_set_matvec = {#set primme_params.matrixMatvec#}


data PrimmeInfo = PrimmeInfo
  { primmeEvals :: Vector Double,
    primmeNorms :: Vector Double,
    primmeTime :: Double
  }
  deriving (Read, Show)

-- void (*monitorFun)(void *basisEvals, int *basisSize, int *basisFlags, int *iblock,
--                    int *blockSize, void *basisNorms, int *numConverged, void *lockedEvals,
--                    int *numLocked, int *lockedFlags, void *lockedNorms, int *inner_its,
--                    void *LSRes, const char *msg, double *time, primme_event *event,
--                    struct primme_params *primme, int *ierr)
type CmonitorFun = Ptr () -> Ptr CInt -> Ptr CInt -> Ptr CInt
                -> Ptr CInt -> Ptr () -> Ptr CInt -> Ptr ()
                -> Ptr CInt -> Ptr CInt -> Ptr () -> Ptr CInt
                -> Ptr () -> Ptr CChar -> Ptr Double -> Ptr ()
                -> Cprimme_params -> Ptr CInt -> IO ()

-- foreign import ccall "wrapper"
--   mkCmonitorFun



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
class ( Storable a, Storable (BlasRealPart a), Floating a, Floating (BlasRealPart a)) => BlasDatatype a where
  -- type BlasRealPart a :: Type
  blasTag :: proxy a -> BlasDatatypeTag a

instance BlasDatatype Float where
  blasTag _ = FloatTag

instance BlasDatatype Double where
  blasTag _ = DoubleTag

instance BlasDatatype (Complex Float) where
  blasTag _ = ComplexFloatTag

instance BlasDatatype (Complex Double) where
  blasTag _ = ComplexDoubleTag

-- class (BlasDatatype a, Storable (BlasRealPart a)) => PrimmeDatatype a where
  -- cDatatype :: Proxy a -> Cprimme_op_datatype
  -- cPrimme :: Ptr (BlasRealPart a) -> Ptr a -> Ptr (BlasRealPart a) -> Cprimme_params -> IO CInt

-- | A safer alternative to 'castPtr'. Typical usage example is converting 'Ptr CFloat' to
-- 'Ptr Float', but making sure we don't accidentally pass 'Ptr CDouble' instead.
castPtr' :: Coercible a b => Ptr a -> Ptr b
castPtr' = castPtr


cPrimme :: forall a. (BlasDatatype a, Storable (BlasRealPart a))
  => Ptr (BlasRealPart a) -> Ptr a -> Ptr (BlasRealPart a) -> Cprimme_params -> IO CInt
cPrimme = case blasTag (Proxy :: Proxy a) of
  FloatTag -> sprimme
  DoubleTag -> dprimme
  ComplexFloatTag -> cprimme
  ComplexDoubleTag -> zprimme

-- instance PrimmeDatatype Float where
  -- cDatatype _ = Cprimme_op_float
  -- cPrimme evals evecs rnorms params = sprimme (castPtr' evals) (castPtr' evecs) (castPtr' rnorms) params

-- instance PrimmeDatatype Double where
  -- cDatatype _ = Cprimme_op_double
  -- cPrimme evals evecs rnorms params = dprimme (castPtr' evals) (castPtr' evecs) (castPtr' rnorms) params

-- instance PrimmeDatatype (Complex Float) where
  -- cDatatype _ = Cprimme_op_float
  -- cPrimme = cprimme

-- instance PrimmeDatatype (Complex Double) where
  -- cDatatype _ = Cprimme_op_float
  -- cPrimme = zprimme

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

hemm' :: forall b. BlasDatatype b => BlasHemmType b
hemm' = case blasTag (Proxy :: Proxy b) of
  FloatTag -> ssymm_
  DoubleTag -> dsymm_
  ComplexFloatTag -> chemm_
  ComplexDoubleTag -> zhemm_
