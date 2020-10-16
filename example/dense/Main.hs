module Main where

import Data.Coerce
import Data.Vector.Storable (MVector (..), Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Numeric.PRIMME

foreign import ccall "dgemv_"
  dgemv_ :: --
    Ptr CChar -> -- 'N', 'T', or 'C'
    Ptr CInt -> -- m
    Ptr CInt -> -- n
    Ptr CDouble -> -- alpha
    Ptr CDouble -> -- A
    Ptr CInt -> -- ldA
    Ptr CDouble -> -- x
    Ptr CInt -> -- incx
    Ptr CDouble -> -- beta
    Ptr CDouble -> -- y
    Ptr CInt -> -- incy
    IO ()

dgemv' :: Int -> Int -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()
dgemv' m n aPtr xPtr yPtr =
  with (fromIntegral m) $ \m' ->
    with (fromIntegral n) $ \n' ->
      with (1 :: CInt) $ \inc' ->
        with (1.0 :: CDouble) $ \α' ->
          with (0.0 :: CDouble) $ \β' ->
            with (fromIntegral (fromEnum 'T') :: CChar) $ \trans' ->
              dgemv_ trans' n' m' α' (castPtr aPtr) n' (castPtr xPtr) inc' β' (castPtr yPtr) inc'

data Matrix = Matrix Int Int (Vector Double)

toPrimme :: Matrix -> PrimmeOperator Double
toPrimme (Matrix n m a) = fromDense n n a

-- PrimmeMatrix gemv n
--   where
--     gemv x y =
--       V.unsafeWith a $ \aPtr ->
--         V.unsafeWith x $ \xPtr ->
--           MV.unsafeWith y $ \yPtr ->
--             dgemv' n m aPtr xPtr yPtr

ex1 :: Matrix
ex1 =
  Matrix 3 3 $
    V.fromList
      [ 3.0,
        2.0,
        1.0,
        2.0,
        4.0,
        5.0,
        1.0,
        5.0,
        3.2
      ]

main :: IO ()
main = do
  putStrLn "Hello world!"
  let m = toPrimme ex1
  --     x = V.fromList [1.0, -2.0, 0.5]
  -- y <- MV.new 4
  -- (pMatrix m) x y
  -- print =<< V.toList <$> V.unsafeFreeze y
  [(evals, evecs, rnorms)] <- eigh (PrimmeOptions 3 1 PrimmeSmallest 1.0e-9) m
  print evals
