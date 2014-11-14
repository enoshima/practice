


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
  | rx == fromListUnboxed (Z:.(0::Int)) [] = do
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



