module FIRfft
(
fftfilt
) where


import Data.Complex()
import Data.List
import Data.Maybe
import qualified Data.Array.Repa as Repa
import Data.Array.Repa
import qualified Data.Array.Repa.Algorithms.FFT as FFT
import qualified Data.Vector.Unboxed as UV
--import qualified Data.Vector.Storable as SV
import qualified Numeric.LinearAlgebra as NL


fftfilt :: UV.Vector Double -> UV.Vector Double -> IO (UV.Vector Double)
fftfilt b x
  | nb >= nx || nx > 2^(20::Int) = do
    let (nfft, _) = helperfunc1 nb nx
        b' = Repa.fromUnboxed (Repa.ix1 nfft)
          (UV.zip (b UV.++ UV.fromList (replicate (nfft-nb) (0::Double)))
          (UV.fromList (replicate nfft (0::Double)))) :: Repa.Array Repa.U Repa.DIM1 (Double, Double)
        x' = Repa.fromUnboxed (Repa.ix1 nfft)
          (UV.zip (x UV.++ UV.fromList (replicate (nfft-nx) (0::Double)))
          (UV.fromList (replicate nfft (0::Double)))) :: Repa.Array Repa.U Repa.DIM1 (Double, Double)

    bfftVal <- FFT.fft1dP FFT.Forward b'
    xfftVal <- FFT.fft1dP FFT.Forward x'
    convbx <- FFT.fft1dP FFT.Inverse $ bfftVal *^ xfftVal
    return (UV.take nx $ snd.UV.unzip.Repa.toUnboxed $ convbx)

  | otherwise = do
    let (nfft, l) = helperfunc2 nb nx
        b' = Repa.fromUnboxed (Repa.ix1 nfft)
          (UV.zip (b UV.++ UV.fromList (replicate (nfft-nb) (0::Double)))
          (UV.fromList (replicate nfft (0::Double)))) :: Repa.Array Repa.U Repa.DIM1 (Double,Double)
        xs' = NL.takesV (replicate nl l) $ UV.convert
          $ x UV.++ UV.fromList (replicate ((nl+1)*l-nx) (0::Double))
        nl = nx `div` l

        xs  = Data.List.map
          ((\v -> genArrayList v nfft l).(\w->UV.zip (UV.convert w) (UV.fromList (replicate l 0))))
          xs'
    bfftVal <- FFT.fft1dP FFT.Forward b'
    outarr <- applyFFT l bfftVal xs
    return (snd.UV.unzip.Repa.toUnboxed $ outarr)
  where
    nb = UV.length b
    nx = UV.length x


helperfunc1 :: Int -> Int -> (Int, Int)
helperfunc1 nb nx = (nextpow2 (nb+nx-1), nx)

helperfunc2 :: Int -> Int -> (Int,  Int)
helperfunc2 nb nx = (nfft, l)
  where
    fftflops = [18,59,138,303,660,1441,3150,6875,14952,32373,69762,149647,
               319644,680105,1441974,3047619,6422736,13500637,28311786,59244791]
    fftflops' = [fftflops!!(x-1)|x<-[1..20],2^x>(nb-1)]
    n = filter (>(nb-1)) [2^x|x<-[(1::Int)..20]]
    llist = [x-(nb-1)|x<-n]
    evalList = Data.List.zipWith (*) [ceiling (nxx x)|x<-llist] fftflops'
    nxx x = fromIntegral nx/fromIntegral x :: Double
    minVal = minimum evalList :: Int
    minIndx = fromMaybe (error "nfft,  L cannot be given.") (elemIndex minVal evalList)
    nfft = n!!minIndx
    l = llist!!minIndx

genArrayList :: UV.Vector (Double,Double) -> Int -> Int -> Repa.Array Repa.U Repa.DIM1 (Double, Double)
genArrayList v nfft l = Repa.fromUnboxed (Repa.ix1 nfft)
  (v UV.++ UV.fromList (zip (replicate (nfft-l) (0::Double)) (replicate (nfft-l) (0::Double))))

applyFFT :: Int
         -> Repa.Array Repa.U Repa.DIM1 (Double, Double)
         -> [Repa.Array Repa.U Repa.DIM1 (Double, Double)]
         -> IO (Repa.Array Repa.U Repa.DIM1 (Double, Double))
applyFFT _ _ [] = return (Repa.fromListUnboxed (Z:.(0::Int)) [])
applyFFT l bfft (arr:arrs) = do
  out <- FFT.fft1dP FFT.Forward arr
  convbx <- FFT.fft1dP FFT.Inverse $ bfft *^ out
  nextconvbx <- applyFFT l bfft arrs
  computeP (Repa.extract (Z :.(0::Int)) (Z :.((l-1)::Int)) convbx Repa.++  nextconvbx)


nextpow2 :: Int -> Int
nextpow2 n = head $ filter (>= n) [2^x|x<-[(1::Int)..]]

--complexUV :: UV.Vector Double -> UV.Vector (Complex Double)
--complexUV v = UV.convert w :: UV.Vector (Complex Double)
--  where w = NL.complex.UV.convert $ v :: SV.Vector (Complex Double)

--doubleUV :: UV.Vector (Complex Double) -> UV.Vector Double
--doubleUV v = UV.convert w :: UV.Vector Double
--  where w = NL.double.UV.convert $ v :: SV.Vector Double


