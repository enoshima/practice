

-- fir0 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
-- fir0 h w []     = y : []
--     where y  = sum [ h!i * w!i | i <- [0..m] ]
--           m  = snd $ bounds h
-- fir0 h w (x:xs) = y : fir0 h w' xs
--     where y  = sum [ h!i * w!i | i <- [0..m] ]
--           w' = listArray (0,m) $ x : elems w
--           m  = snd $ bounds h
--

import qualified Data.Array.Repa as Repa
import Data.Array.Repa
import qualified Data.Array.Repa.Repr.Unboxed as RRU
-- import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
-- import qualified Numeric.LinearAlgebra as NL
import System.Random
import HasKAL.SignalProcessingUtils.ButterWorth

main :: IO (Array U DIM1 Double)
main = do

  let datalen = 1024 :: Int
      vx = VU.fromList $ Prelude.take datalen $ randomRs (-1,1) $ mkStdGen 1
      vh = VU.fromList $ snd $ butter 6 1024 50 High
      vw = VU.fromList $ (vx VU.! 0):Prelude.replicate 6 0

      x = RRU.fromUnboxed (Repa.ix1 datalen) vx
      h = RRU.fromUnboxed (Repa.ix1 6) vh
      w = RRU.fromUnboxed (Repa.ix1 7) vw

  fir0repa h w x



fir0repa :: Repa.Array Repa.U Repa.DIM1 Double
          -> Repa.Array Repa.U Repa.DIM1 Double
          -> Repa.Array Repa.U Repa.DIM1 Double
          -> IO (Repa.Array Repa.U Repa.DIM1 Double)
fir0repa rh rw rx
  | rx == fromListUnboxed (Z:.(1::Int)) [] = do
    y' <- computeP (rh *^ rw) :: IO (Array U DIM1 Double)
    sumy <- sumAllP y'
    return (fromListUnboxed (Z :.(1::Int)) [sumy])  :: IO (Array U DIM1 Double)
  | otherwise = do
    y' <- computeP (rh *^ rw) :: IO (Array U DIM1 Double)
    sumy <- sumAllP y'
    let y = fromListUnboxed (Z :.(1::Int)) [sumy] :: Array U DIM1 Double
    rw' <- computeP (extract (Z:.(0::Int)) (Z:.(1::Int)) rx
      Repa.++ extract (Z :.(1::Int)) (Z :.(size (extent rw)-1)) rw) :: IO (Array U DIM1 Double)
    rx' <- computeP (extract (Z:.(1::Int)) (Z:.(size (extent rx)-1)) rx) :: IO (Array U DIM1 Double)
    output <- fir0repa rh rw' rx' :: IO (Array U DIM1 Double)
    computeP (y Repa.++ output) :: IO (Array U DIM1 Double)
