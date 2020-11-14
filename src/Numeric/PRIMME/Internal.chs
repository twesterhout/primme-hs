module Numeric.PRIMME.Internal
  ( PrimmeException (..),
    checkError,
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
    primme_set_max_basis_size,
    primme_set_max_block_size,
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

import Control.Exception (Exception, throw, bracket)
import Control.Monad
import Control.Monad.ST (RealWorld)
import Data.Complex (Complex)
import Data.Coerce
import Data.Proxy
import Data.Kind
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Storable (MVector, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.Storable
import Foreign.C.Types (CChar, CInt, CFloat, CDouble, CLong)
import Foreign.Ptr (Ptr, castPtr, nullPtr, FunPtr, freeHaskellFunPtr)
import Foreign.Marshal.Utils

#include <primme.h>
#include "wrapper.h"

getErrorMessage :: CInt -> Text
getErrorMessage c = case (- c) of
  0 -> "success"
  1 ->
    "unexpected internal error; please, consider setting a higher verbosity level to get "
      <> "the stacktrace and report it, because it might be a bug"
  2 -> "memory allocation failed"
  3 ->
    "maximum number of iterations reached (either maximum number of outer iterations or "
      <> "maximum number of matrix-vector products)"
  5 -> "negative operator dimension"
  10 -> "requested number of eigenpairs exceeds the operator dimension"
  11 -> "number of eigenpairs is negative"
  17 -> "maximum basis size is too small"
  19 ->
    "maximum block size is negative, or maximum block size is zero but requested "
      <> "number of eigenpairs is positive"
  23 -> "number of initial vectors exceeds the maximum basis size"
  24 -> "number of initial vectors exceeds the number of requested eigenpairs"
  40 -> "some LAPACK factorization function failed; try increasing verbosity to see the stacktrace"
  41 -> "some user defined function failed; try increasing verbosity to see the stacktrace"
  _ ->
    "primme failed with error code " <> T.pack (show (- c)) <> "; this should not have "
      <> "happened and it is likely a bug in primme-hs package; please, report it"

-- | Exceptions thrown by this package.
newtype PrimmeException = PrimmeException Text
  deriving (Show)

instance Exception PrimmeException

checkError :: CInt -> IO CInt
checkError c
  | c == 0 = return c
  | otherwise = throw $ PrimmeException (getErrorMessage c)

type PrimmeInt = {#type wrap_primme_int#}

{#enum primme_target as Cprimme_target {} add prefix = "C" #}
{#enum primme_projection as Cprimme_projection {} add prefix = "C" #}
{#enum primme_op_datatype as Cprimme_op_datatype {} add prefix = "C" #}
{#enum primme_preset_method as Cprimme_preset_method {}
  with prefix = "PRIMME_" add prefix = "Cprimme_" #}

{#pointer *primme_params as Cprimme_params#}

{#fun unsafe primme_params_create { } -> `Cprimme_params' check* #}
  where
    check ptr
      | ptr == nullPtr = throw $ PrimmeException "failed to allocate primme_params struct"
      | otherwise = return ptr
{#fun unsafe primme_params_destroy { `Cprimme_params' } -> `()' checkError*- #}
{#fun unsafe primme_set_method { `Cprimme_preset_method', `Cprimme_params' } -> `()' checkError*- #}
-- {#fun wrap_primme_display_params as primme_display_params { `Cprimme_params' } -> `()' #}

primme_set_dim :: Cprimme_params -> Int -> IO ()
primme_set_dim p = {#set primme_params.n#} p . fromIntegral

primme_get_dim :: Cprimme_params -> IO Int
primme_get_dim p = fromIntegral <$> {#get primme_params.n#} p

primme_set_print_level :: Cprimme_params -> Int -> IO ()
primme_set_print_level p = {#set primme_params.printLevel#} p . fromIntegral

primme_set_num_evals :: Cprimme_params -> Int -> IO ()
primme_set_num_evals p = {#set primme_params.numEvals#} p . fromIntegral

primme_set_target :: Cprimme_params -> Cprimme_target -> IO ()
primme_set_target p t = {#set primme_params.target#} p (fromIntegral . fromEnum $ t)

primme_set_eps :: Cprimme_params -> Double -> IO ()
primme_set_eps p eps = {#set primme_params.eps#} p (coerce eps)

primme_set_max_basis_size :: Cprimme_params -> Int -> IO ()
primme_set_max_basis_size p = {#set primme_params.maxBasisSize#} p . fromIntegral

primme_set_max_block_size :: Cprimme_params -> Int -> IO ()
primme_set_max_block_size p = {#set primme_params.maxBlockSize#} p . fromIntegral

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

{#fun sprimme { castPtr' `Ptr Float', castPtr' `Ptr Float', castPtr' `Ptr Float', `Cprimme_params' } -> `CInt' id #}
{#fun dprimme { castPtr' `Ptr Double', castPtr' `Ptr Double', castPtr' `Ptr Double', `Cprimme_params' } -> `CInt' id #}
-- C2HS refuses to handle C99 complex types properly
foreign import ccall "cprimme"
  cprimme :: Ptr Float -> Ptr (Complex Float) -> Ptr Float -> Cprimme_params -> IO CInt

foreign import ccall "zprimme"
  zprimme :: Ptr Double -> Ptr (Complex Double) -> Ptr Double -> Cprimme_params -> IO CInt



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
