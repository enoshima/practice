


module FIRr
(
  fir0repa
) where

import qualified Data.Array.Repa as Repa
import Data.Array.Repa



fir0repa :: Repa.Array Repa.U Repa.DIM1 Double
          -> Repa.Array Repa.U Repa.DIM1 Double
          -> Repa.Array Repa.U Repa.DIM1 Double
          -> IO (Repa.Array Repa.U Repa.DIM1 Double)
fir0repa rh rw rx
  | rx == fromListUnboxed (Z:.0) [] = do
    y' <- computeP (rh *^ rw) :: IO (Array U DIM1 Double)
    sumy <- sumAllP y'
    return (fromListUnboxed (Z:.1) [sumy])  :: IO (Array U DIM1 Double)
  | otherwise = do
    y' <- computeP (rh *^ rw) :: IO (Array U DIM1 Double)
    sumy <- sumAllP y'
    let y = fromListUnboxed (Z:.1) [sumy] :: Array U DIM1 Double
    rw' <- computeP (extract (Z:.0) (Z:.1) rx
      Repa.++ extract (Z:.1) (Z:.(size (extent rw)-1)) rw) :: IO (Array U DIM1 Double)
    rx' <- computeP (extract (Z:.1) (Z:.(size (extent rx)-1)) rx) :: IO (Array U DIM1 Double)
    output <- fir0repa rh rw' rx' :: IO (Array U DIM1 Double)
    computeP (y Repa.++ output) :: IO (Array U DIM1 Double)



