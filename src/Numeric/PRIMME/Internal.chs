module Numeric.PRIMME.Internal where

import Control.Monad
import Data.Coerce
import Data.Proxy
import Foreign.C.Types (CInt, CLong)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

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

class PrimmeDatatype a where
  toPrimmeDatatype :: Proxy a -> Cprimme_op_datatype

instance PrimmeDatatype Float where toPrimmeDatatype _ = Cprimme_op_float


{#pointer *primme_params as Cprimme_params#}

{#fun unsafe primme_params_create { } -> `Cprimme_params' #}
{#fun unsafe primme_params_destroy { `Cprimme_params' } -> `CInt' #}
{#fun unsafe primme_set_method { `Cprimme_preset_method', `Cprimme_params' } -> `CInt' #}
{#fun wrap_primme_display_params as primme_display_params { `Cprimme_params' } -> `()' #}

primme_set_dim :: Cprimme_params -> Int -> IO ()
primme_set_dim p n
  | n <= 0 = error $ "invalid matrix dimension: " <> show n
  | otherwise = {#set primme_params.n#} p (fromIntegral n)

primme_set_num_evals :: Cprimme_params -> Int -> IO ()
primme_set_num_evals p n
  | n <= 0 = invalidArgument
  | otherwise = do
      dim <- fromIntegral <$> {#get primme_params.n#} p
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
