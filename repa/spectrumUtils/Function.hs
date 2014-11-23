
-- {-# OPTIONS_GHC -XBangPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Function
( gwpsd
--
) where

import qualified Data.Vector.Unboxed as UV
import WindowType
import WindowFunction
import HasKAL.SpectrumUtils.GwPsdMethod
import qualified Data.Array.Repa as Repa
import Data.Array.Repa
import qualified Data.Array.Repa.Algorithms.FFT as FFT
--import qualified Control.Monad as M
--import Data.Array.Repa.Algorithms.Complex
--import qualified Data.List as L


gwpsd :: UV.Vector Double -> Int -> Double -> IO(UV.Vector (Double, Double))
gwpsd v nfft fs = gwpsdCore Welch v nfft fs Hann


gwpsdCore :: PSDMETHOD -> UV.Vector Double -> Int -> Double -> WindowType -> IO (UV.Vector (Double, Double))
gwpsdCore method v nfft fs w
  | method==Welch = gwpsdWelch v nfft fs w
  | method==MedianAverage = gwpsdMedianAverageCore v nfft fs w
  | otherwise = error "No such method implemented. Check GwPsdMethod.hs"


gwpsdWelch :: UV.Vector Double -> Int -> Double -> WindowType -> IO (UV.Vector (Double, Double))
gwpsdWelch v nfft fs w = do

  let !nv = UV.length v
      !maxitr = floor $ fromIntegral nv / fromIntegral nfft :: Int
      !wvlist = Prelude.map (applyWindow w) (takesV (replicate maxitr nfft) v)
  !oneArry <- go wvlist
  !arry' <- computeP $ reshape (Z:.nfft:.maxitr) oneArry :: IO (Array U DIM2 Double)
  !arry <- computeP (Repa.transpose arry') :: IO (Array U DIM2 Double)
  !sumArry <- Repa.sumP arry :: IO(Array U DIM1 Double)
  !out <- Repa.computeP (Repa.map (1.0/(fromIntegral nfft * fs)*) sumArry)
  let !ftrain = UV.fromList $ Prelude.map (fs/fromIntegral nfft *) [0..fromIntegral nfft-1.0]
  return $ UV.zip (toUnboxed out) ftrain
  where
    go [] = return $ Repa.fromListUnboxed (Z:.0) []
    go (x:xs) = do
      let !xv = Repa.fromUnboxed (Repa.ix1 nfft)
            $ UV.zip x (UV.fromList (replicate nfft (0::Double))) :: Array U DIM1 (Double, Double)
      !ffted <- FFT.fft1dP FFT.Forward xv
      !arry <- Repa.computeP (Repa.map (\(z,y)->z**2.0+y**2.0) ffted) :: IO (Array U DIM1 Double)
      !nextarry <- go xs
      computeP (arry Repa.++ nextarry)
    applyWindow :: WindowType -> UV.Vector Double -> UV.Vector Double
    applyWindow windowtype
      | windowtype==Hann = windowed (hanning nfft)
      | otherwise = error "No such window implemented. Check WindowType.hs"


gwpsdMedianAverageCore :: UV.Vector Double -> Int -> Double -> WindowType -> IO(UV.Vector (Double, Double))
gwpsdMedianAverageCore v nfft fs w = undefined



takesV :: [Int] -> UV.Vector Double -> [UV.Vector Double]
takesV ms w | sum ms > UV.length w = error $ "takesV " Prelude.++ show ms Prelude.++ " on dim = " Prelude.++ (show $ UV.length w)
            | otherwise = go ms w
            where
              go [] _ = []
              go (n:ns) v = UV.slice 0 n v : go ns (UV.slice n (UV.length v - n) v)

