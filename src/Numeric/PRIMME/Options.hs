{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Numeric.PRIMME.Options
  ( primmeDefaults,
    withOptions,
    finalizeOptions,
  )
where

import Control.Exception.Safe (bracket)
import Control.Monad (when)
import Data.Coerce
import Data.Proxy
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Numeric.PRIMME.Context
import Numeric.PRIMME.Monitor
import Numeric.PRIMME.Operator
import Numeric.PRIMME.Types

C.context (C.baseCtx <> C.funCtx <> primmeCtx)
C.include "<primme.h>"
C.include "wrapper.h"

primmeDefaults :: PrimmeOptions
primmeDefaults =
  PrimmeOptions
    { pDim = -1,
      pNumEvals = 1,
      pTarget = PrimmeSmallest,
      pMethod = PrimmeDynamic,
      pEps = 0,
      pMaxBasisSize = 0,
      pMinRestartSize = 0,
      pMaxBlockSize = 1,
      pLogAction = Nothing
    }

initOptions :: PrimmeOptions -> Ptr Cprimme_params -> IO ()
initOptions options params
  | pDim options == -1 =
    error $
      "pDim in PrimmeOptions is equal to the default value -1; please, set it "
        <> "to the dimension of your operator prior to calling eigh"
  | pDim options <= 0 =
    error $
      "pDim in PrimmeOptions is invalid: " <> show (pDim options) <> "; expected a positive integer"
  | pNumEvals options <= 0 =
    error $
      "pNumEvals in PrimmeOptions is invalid: " <> show (pNumEvals options) <> "; expected a positive integer"
  | pNumEvals options > pDim options =
    error $
      "pNumEvals in PrimmeOptions is invalid: " <> show (pNumEvals options)
        <> "; number of eigenpairs cannot exceed the dimension of the operator"
  | pEps options < 0 =
    error $
      "pEps in PrimmeOptions is invalid: " <> show (pEps options) <> "; expected a non-negative number"
  | pMaxBasisSize options /= 0 && pMaxBasisSize options < 2 && pDim options > 2 =
    error $
      "pMaxBasisSize in PrimmeOptions is invalid: " <> show (pMaxBasisSize options)
        <> "; basis size must be at least 2"
  | pMinRestartSize options < 0 =
    error $
      "pMinRestartSize in PrimmeOptions is invalid: " <> show (pMinRestartSize options)
        <> "; expected a positive integer"
  | pMaxBlockSize options <= 0 =
    error $
      "pMaxBlockSize in PrimmeOptions is invalid: " <> show (pMaxBlockSize options)
        <> "; expected a positive integer"
  | otherwise = do
    let c_n = fromIntegral $ pDim options
        c_numEvals = fromIntegral $ pNumEvals options
        c_target = case pTarget options of
          PrimmeSmallest -> [CU.pure| primme_target { primme_smallest } |]
          PrimmeLargest -> [CU.pure| primme_target { primme_largest } |]
        c_eps = coerce (pEps options)
        c_maxBasisSize = fromIntegral $ pMaxBasisSize options
        c_minRestartSize = fromIntegral $ pMinRestartSize options
        c_maxBlockSize = fromIntegral $ pMaxBlockSize options
        c_method = case pMethod options of
          PrimmeDynamic -> [CU.pure| primme_preset_method { PRIMME_DYNAMIC } |]
          PrimmeGDOlsenPlusK -> [CU.pure| primme_preset_method { PRIMME_GD_Olsen_plusK } |]
          PrimmeJDQMRETol -> [CU.pure| primme_preset_method { PRIMME_JDQMR_ETol } |]
    status <-
      [CU.block| int {
      $(primme_params* params)->n = $(int64_t c_n);
      $(primme_params* params)->numEvals = $(int c_numEvals);
      $(primme_params* params)->target = $(primme_target c_target);
      $(primme_params* params)->eps = $(double c_eps);
      $(primme_params* params)->maxBasisSize = $(int c_maxBasisSize);
      $(primme_params* params)->minRestartSize = $(int c_minRestartSize);
      $(primme_params* params)->maxBlockSize = $(int c_maxBlockSize);
      $(primme_params* params)->printLevel = 5;
      return primme_set_method($(primme_preset_method c_method), $(primme_params* params));
    } |]
    when (status /= 0) . error $
      "primme_set_method failed with error code: " <> show status

getFinalOptions :: PrimmeOptions -> Ptr Cprimme_params -> IO PrimmeOptions
getFinalOptions options params = do
  initOptions options params
  eps <- coerce <$> [CU.exp| double { $(primme_params* params)->eps } |]
  maxBasisSize <- fromIntegral <$> [CU.exp| int { $(primme_params* params)->maxBasisSize } |]
  minRestartSize <- fromIntegral <$> [CU.exp| int { $(primme_params* params)->minRestartSize } |]
  maxBlockSize <- fromIntegral <$> [CU.exp| int { $(primme_params* params)->maxBlockSize } |]
  return $
    options
      { pEps = eps,
        pMaxBasisSize = maxBasisSize,
        pMinRestartSize = minRestartSize,
        pMaxBlockSize = maxBlockSize
      }

primme_params_create :: IO (Ptr Cprimme_params)
primme_params_create = [CU.exp| primme_params* { primme_params_create() } |]

primme_params_destroy :: Ptr Cprimme_params -> IO ()
primme_params_destroy params = [CU.exp| void { primme_params_destroy($(primme_params* params)) } |]

finalizeOptions :: PrimmeOptions -> IO PrimmeOptions
finalizeOptions options =
  bracket primme_params_create primme_params_destroy $
    getFinalOptions options

withOptions :: forall a b. BlasDatatype a => PrimmeOptions -> PrimmeOperator a -> (Ptr Cprimme_params -> IO b) -> IO b
withOptions options matvec action =
  bracket primme_params_create primme_params_destroy $ \params -> do
    initOptions options params
    withOperator matvec $ \matvecPtr -> do
      [CU.exp| void { $(primme_params* params)->matrixMatvec =
        $(void (*matvecPtr)(void*, PRIMME_INT*, void*, PRIMME_INT*, int*, primme_params*, int*))
      } |]
      case pLogAction options of
        Just monitor -> withMonitor (Proxy :: Proxy a) monitor $ \monitorPtr -> do
          [CU.block| void {
            $(primme_params* params)->monitorFun =
                $(void (*monitorPtr)(void*, int*, int*, int*, int*, void*, int*, void*, int*,
                                     int*, void*, int*, void*, const char*, double*,
                                     primme_event*, primme_params*, int*));
            $(primme_params* params)->printLevel = 0;
          } |]
          action params
        Nothing -> action params
