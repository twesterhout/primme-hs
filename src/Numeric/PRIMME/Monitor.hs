-- |
-- Copyright: (c) 2020 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module Numeric.PRIMME.Monitor
  ( PrimmeMonitor (..),
    PrimmeInfo (..),
    PrimmeEventInfo (..),
    PrimmeStats (..),
    withCmonitorFun,
  )
where

import Control.Exception.Safe (bracket)
import Control.Monad
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
import Numeric.PRIMME.Internal

foreign import ccall "wrapper"
  mkCmonitorFun :: CmonitorFun () -> IO (FunPtr (CmonitorFun ()))

withCmonitorFun :: forall a r. BlasDatatype a => Proxy a -> PrimmeMonitor -> (FunPtr (CmonitorFun ()) -> IO r) -> IO r
withCmonitorFun _ f = bracket (mkCmonitorFun monitorImpl) freeHaskellFunPtr
  where
    monitorImpl :: CmonitorFun ()
    monitorImpl
      basisEvalsPtr
      basisSizePtr
      _ {-basisFlagsPtr-}
      iblockPtr
      blockSizePtr
      basisNormsPtr
      _ {-numConvergedPtr-}
      _ {-lockedEvalsPtr-}
      _ {-numLockedPtr-}
      _ {-lockedFlagsPtr-}
      _ {-lockedNormsPtr-}
      _ {-innter_itsPtr-}
      _ {-lsresPtr-}
      msgPtr
      _ {-timePtr-}
      eventPtr
      _ {-paramsPtr-}
      ierrPtr = do
        basisSize <- fromIntegral <$> peek basisSizePtr
        blockSize <- fromIntegral <$> peek blockSizePtr
        eventInfo <-
          peek (castPtr eventPtr :: Ptr Cprimme_event) >>= \event -> case event of
            Cprimme_event_outer_iteration -> mkOuterInfo @a basisEvalsPtr basisSize iblockPtr blockSize basisNormsPtr
            Cprimme_event_inner_iteration -> pure PrimmeInnerInfo
            Cprimme_event_locked -> pure PrimmeLockedInfo
            Cprimme_event_converged -> pure PrimmeConvergedInfo
            Cprimme_event_message -> mkMessageInfo msgPtr
            _ -> error $ "unexpected event: " <> show event
        let stats = PrimmeStats
        let info = PrimmeInfo eventInfo stats
        shouldStop <- unPrimmeMonitor f info
        pure ()

-- print "Done!"
-- when shouldStop $ do
--   poke ierrPtr 1

newtype PrimmeMonitor = PrimmeMonitor
  { unPrimmeMonitor :: forall a. BlasDatatype a => PrimmeInfo a -> IO Bool
  }

data PrimmeInfo a = PrimmeInfo (PrimmeEventInfo a) PrimmeStats
  deriving (Show)

data PrimmeStats = PrimmeStats
  deriving (Show)

data PrimmeEventInfo a
  = PrimmeOuterInfo (Vector a) (Vector a)
  | PrimmeInnerInfo
  | PrimmeLockedInfo
  | PrimmeConvergedInfo
  | PrimmeMessageInfo !Text
  deriving (Show)

loadVector :: (Storable a, Integral n, Show n) => Ptr a -> n -> IO (Vector a)
loadVector dataPtr n
  | n < 0 = error $ "invalid 'n': " <> show n
  | otherwise = V.freeze =<< MV.unsafeFromForeignPtr0 <$> newForeignPtr_ dataPtr <*> pure (fromIntegral n)

mkOuterInfo :: forall a. Storable a => Ptr () -> Int -> Ptr CInt -> Int -> Ptr () -> IO (PrimmeEventInfo a)
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

mkMessageInfo :: Ptr CChar -> IO (PrimmeEventInfo a)
mkMessageInfo msgPtr = PrimmeMessageInfo . T.pack <$> peekCString msgPtr
