-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module Numeric.PRIMME.Monitor
  ( withMonitor,
    primmePrettyInfo,
  )
where

import Control.Exception.Safe (bracket)
import Data.Coerce
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar, CInt)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr)
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Numeric.PRIMME.Context
import Numeric.PRIMME.Types
import Text.Printf

C.context (C.baseCtx <> primmeCtx)
C.include "<primme.h>"

data PrimmeEventType
  = OuterIterationTy
  | InnerIterationTy
  | LockedTy
  | ConvergedTy
  | MessageTy

instance Enum PrimmeEventType where
  toEnum x
    | x == primme_event_outer_iteration = OuterIterationTy
    | x == primme_event_inner_iteration = InnerIterationTy
    | x == primme_event_locked = LockedTy
    | x == primme_event_converged = ConvergedTy
    | x == primme_event_message = MessageTy
    | otherwise = error $ "unexpected primme_event: " <> show x
    where
      primme_event_outer_iteration = fromIntegral [CU.pure| primme_event { primme_event_outer_iteration } |]
      primme_event_inner_iteration = fromIntegral [CU.pure| primme_event { primme_event_inner_iteration } |]
      primme_event_locked = fromIntegral [CU.pure| primme_event { primme_event_locked } |]
      primme_event_converged = fromIntegral [CU.pure| primme_event { primme_event_converged } |]
      primme_event_message = fromIntegral [CU.pure| primme_event { primme_event_message } |]
  fromEnum _ = error "fromEnum not implemented for PrimmeEventType"

foreign import ccall "wrapper"
  mkCmonitorFun :: CmonitorFun -> IO (FunPtr CmonitorFun)

withMonitor :: forall a r. BlasDatatype a => Proxy a -> PrimmeMonitor -> (FunPtr CmonitorFun -> IO r) -> IO r
withMonitor _ f = bracket (mkCmonitorFun monitorImpl) freeHaskellFunPtr
  where
    -- monitorImpl :: CmonitorFun ()
    monitorImpl
      basisEvalsPtr
      basisSizePtr
      _ {-basisFlagsPtr-}
      iblockPtr
      blockSizePtr
      basisNormsPtr
      numConvergedPtr
      _ {-lockedEvalsPtr-}
      _ {-numLockedPtr-}
      _ {-lockedFlagsPtr-}
      _ {-lockedNormsPtr-}
      _ {-innter_itsPtr-}
      _ {-lsresPtr-}
      msgPtr
      _ {-timePtr-}
      eventPtr
      params
      ierrPtr = do
        basisSize <- fromIntegral <$> peek basisSizePtr
        blockSize <- fromIntegral <$> peek blockSizePtr
        eventInfo <-
          peek eventPtr >>= \event -> case (toEnum . fromIntegral $ event) of
            OuterIterationTy -> mkOuterInfo @a basisEvalsPtr basisSize iblockPtr blockSize basisNormsPtr
            InnerIterationTy -> pure PrimmeInnerInfo
            LockedTy -> pure PrimmeLockedInfo
            ConvergedTy -> mkConvergedInfo @a basisEvalsPtr iblockPtr basisNormsPtr numConvergedPtr
            MessageTy -> mkMessageInfo msgPtr
        stats <- loadStats params
        let info = PrimmeInfo eventInfo stats
        shouldStop <- unPrimmeMonitor f info
        if shouldStop
          then poke ierrPtr 1
          else poke ierrPtr 0

loadVector :: (Storable a, Integral n, Show n) => Ptr a -> n -> IO (Vector a)
loadVector dataPtr n
  | n < 0 = error $ "invalid 'n': " <> show n
  | otherwise = V.freeze =<< MV.unsafeFromForeignPtr0 <$> newForeignPtr_ dataPtr <*> pure (fromIntegral n)

loadStats :: Ptr Cprimme_params -> IO PrimmeStats
loadStats p =
  PrimmeStats
    <$> (fromIntegral <$> [CU.exp| PRIMME_INT { $(primme_params* p)->stats.numOuterIterations } |])
    <*> (fromIntegral <$> [CU.exp| PRIMME_INT { $(primme_params* p)->stats.numRestarts } |])
    <*> (fromIntegral <$> [CU.exp| PRIMME_INT { $(primme_params* p)->stats.numMatvecs } |])
    <*> (coerce <$> [CU.exp| double { $(primme_params* p)->stats.timeMatvec } |])
    <*> (coerce <$> [CU.exp| double { $(primme_params* p)->stats.timeOrtho } |])
    <*> (coerce <$> [CU.exp| double { $(primme_params* p)->stats.timeDense } |])

mkOuterInfo :: forall a. BlasDatatype a => Ptr () -> Int -> Ptr CInt -> Int -> Ptr () -> IO (PrimmeEventInfo a)
mkOuterInfo
  basisEvalsPtr
  basisSize
  iblockPtr
  blockSize
  basisNormsPtr = do
    iblock <- V.map fromIntegral <$> loadVector iblockPtr blockSize
    basisEvals <- loadVector (castPtr basisEvalsPtr) basisSize
    basisNorms <- loadVector (castPtr basisNormsPtr) basisSize
    let blockEvals = V.map (\i -> basisEvals V.! i) iblock
    let blockNorms = V.map (\i -> basisNorms V.! i) iblock
    pure $ PrimmeOuterInfo blockEvals blockNorms

mkConvergedInfo :: BlasDatatype a => Ptr () -> Ptr CInt -> Ptr () -> Ptr CInt -> IO (PrimmeEventInfo a)
mkConvergedInfo basisEvalsPtr iblockPtr basisNormsPtr numConvergedPtr = do
  numConverged <- fromIntegral <$> peek numConvergedPtr
  i <- fromIntegral <$> peek iblockPtr
  eval <- peekElemOff (castPtr basisEvalsPtr) i
  norm <- peekElemOff (castPtr basisNormsPtr) i
  pure $ PrimmeConvergedInfo numConverged eval norm

mkMessageInfo :: Ptr CChar -> IO (PrimmeEventInfo a)
mkMessageInfo msgPtr = PrimmeMessageInfo . T.pack <$> peekCString msgPtr

primmePrettyInfo :: BlasDatatype a => PrimmeInfo a -> Text
primmePrettyInfo (PrimmeInfo (PrimmeOuterInfo blockEvals blockNorms) stats) =
  T.pack . mconcat $ [msgHeader, msgEvals, msgNorms, msgTimings]
  where
    msgHeader = printf "Processed block %d/%d: " (pStatsNumRestarts stats) (pStatsNumOuterIterations stats)
    msgEvals = if V.null blockEvals then mempty else mconcat ["eigenvalues ", show blockEvals, ", "]
    msgNorms = if V.null blockNorms then mempty else mconcat ["residual norms ", show blockNorms, ", "]
    timeTotal = pStatsTimeMatvec stats + pStatsTimeOrtho stats + pStatsTimeDense stats
    timeMatvec = 100 * (pStatsTimeMatvec stats / timeTotal)
    timeOrtho = 100 * (pStatsTimeOrtho stats / timeTotal)
    timeDense = 100 - timeMatvec - timeOrtho
    timePerMatvec = pStatsTimeMatvec stats / fromIntegral (pStatsNumMatvecs stats)
    msgTimings =
      printf
        "%.0f%% in matvec, %.0f%% in orthogonalization, %.0f%% in dense linear algebra, time per matrix-vector %.1e s"
        timeMatvec
        timeOrtho
        timeDense
        timePerMatvec
primmePrettyInfo (PrimmeInfo (PrimmeConvergedInfo i eval norm) _) =
  T.pack . mconcat $ ["Converged eigenvalue ", show i, ": ", show eval, ", residual norm ", show norm]
primmePrettyInfo (PrimmeInfo info _) = T.pack $ show info
