module Main (main) where

import Control.Monad (forM_, unless)
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable)
import Numeric.PRIMME

ex1 :: Vector Double
ex1 =
  V.fromList . concat . transpose $
    [ [3.0, 2.0, 1.0],
      [2.0, 4.0, 5.0],
      [1.0, 5.0, 3.2]
    ]

solution1 :: (Vector Double, Block Double)
solution1 = (values, vectors)
  where
    values = V.fromList [-1.5113319965715168, 2.3643749026322247, 9.346957093939288]
    vectors =
      Block (3, 3) 3 . V.fromList . concat . transpose $
        [ [0.1514845272234791, -0.9344973092419614, -0.3221291930754075],
          [-0.693750555046281, 0.1316243126612886, -0.7080855934768199],
          [0.7041041154437716, 0.33074131785426725, -0.6283689802012562]
        ]

isCloseFloating' :: (Ord a, Floating a) => a -> a -> a -> a -> Bool
isCloseFloating' rTol aTol a b = aTol + rTol * max (abs a) (abs b) > abs (a - b)

isCloseFloating :: (Ord a, Floating a) => a -> a -> Bool
isCloseFloating = isCloseFloating' 1.0e-7 1.0e-10

isCloseVector :: (Ord a, Floating a, Storable a) => Vector a -> Vector a -> Bool
isCloseVector a b = V.all id $ V.zipWith isCloseFloating a b

dot :: (Num a, Storable a) => Block a -> Block a -> a
dot (Block aShape aStride aData) (Block bShape bStride bData)
  | fst aShape == aStride && fst bShape == bStride = V.foldl' (+) 0 $ V.zipWith (*) aData bData
  | otherwise = error "non-contiguous Blocks are not yet supported"

ex2 :: Vector Double
ex2 =
  V.fromList . concat . transpose $
    [ [0.10554899942443097, -0.16311772004894198, 0.23762263426873975, -0.720377316529844, -0.5381199365306265],
      [-0.16311772004894198, 0.15532475089066877, -0.12165468345908137, -0.9811658380492235, -0.047234044072823944],
      [0.23762263426873975, -0.12165468345908137, 0.4919422209022195, -0.014224262950043087, 0.5276559662526343],
      [-0.720377316529844, -0.9811658380492235, -0.014224262950043087, -0.18616049236123877, 0.46098380976114095],
      [-0.5381199365306265, -0.047234044072823944, 0.5276559662526343, 0.46098380976114095, 0.8132577333875255]
    ]

solution2 :: (Vector Double, Block Double)
solution2 = (values, vectors)
  where
    values = V.fromList [-1.3270592970280437, -0.37156539032819186, 0.5931686225674098, 0.8192103673252326, 1.6661589097071987]
    vectors =
      Block (3, 3) 3 . V.fromList . concat . transpose $
        [ [-0.41521949308114414, 0.5861105972845077, 0.514456651784022, -0.26740905865910647, 0.38456974542787414],
          [-0.5319447141274747, -0.1908405652211378, -0.6520896176572546, -0.3968496193627089, 0.3128964843733538],
          [0.0006034108610660248, -0.5317998335656995, 0.444566558507755, -0.6749724588809534, -0.2529057684664351],
          [-0.7367694253454836, -0.1608474278423213, 0.16441616375736115, 0.41488888390530004, -0.48180230171692007],
          [0.04240334242362954, 0.5580065307797732, -0.29230358213794294, -0.37852631179712976, -0.6768361066794644]
        ]

asOperator :: BlasDatatype a => Vector a -> (Int, PrimmeOperator a)
asOperator matrix
  | dim * dim == size = (dim, primmeFromDense (Block (dim, dim) dim matrix))
  | otherwise = error "matrix is not square"
  where
    size = V.length matrix
    dim = round . (sqrt :: Double -> Double) . fromIntegral $ size

logAction :: PrimmeMonitor
logAction = PrimmeMonitor helper
  where
    helper :: BlasDatatype a => PrimmeInfo a -> IO Bool
    helper info = do
      T.putStrLn $ primmePrettyInfo info
      pure False

main :: IO ()
main = do
  forM_ [(ex1, solution1), (ex2, solution2)] $ \(ex, solution) -> do
    let (dim, operator) = asOperator ex
    let o =
          primmeDefaults
            { pDim = dim,
              pNumEvals = dim,
              pTarget = PrimmeSmallest,
              pLogAction = Right logAction,
              pEps = 1.0e-9
            }
    (evals, evecs, rnorms) <- eigh o operator
    unless (isCloseVector evals (fst solution)) $
      error $ "Incorrect eigenvalues: " <> show evals <> " != " <> show (fst solution)
    let overlap = abs $ dot evecs (snd solution)
    unless (isCloseFloating overlap 1) $
      error $ "Incorrect eigenvectors: " <> show evecs <> " != " <> show (snd solution) <> "; overlap = " <> show overlap
    print evals
    print rnorms
