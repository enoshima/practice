{-
 - unit test of whitening filter based on linear prediction error filter
 - for compilation
 - ghc--make testLPEF.hs
 -}

import HasKAL.SignalProcessingUtils.ButterWorth
--import HasKAL.SignalProcessingUtils.Chebyshev
--import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.LinearPrediction
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.FrameUtils.FrameUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import Data.List
import Filter
import Resampling
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier

main = do


  {- input data information -}
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname,  _)]=ch
  fdata <- readFrame chname fname
  fs' <- getSamplingFrequency fname chname

  {- downsampling -}
  let fs = 4096 :: Double
      x = downsample fs' fs $ map realToFrac (eval fdata)

  {- whitening filter properties -}
  let nfft = truncate fs
      nC =2048-1
      df = fs/(fromIntegral nC+1)
      fc = 64
      nhpf = 4
--      r = 0.5

  {- high-pass filter coefficients -}
--  let hpf = chebyshev1 nhpf r fs fc High
  let hpf = butter nhpf fs fc High
  {- high-pass filterd -}
--      x_hpf = iir_df2 hpf x
      x_hpf = x



  let anlylen = (truncate fs) * 5
      trlen = (truncate fs)

      {- set training data -}
      trdat = take trlen x_hpf
      {- whitening filter coefficients -}
      (whnb,rho)= lpefCoeff nC (gwpsd trdat nfft fs)
--      whnb = map (fs*) whnb'
      {- transfer function of whitening filter -}
      psd_whnb = gwpsd (whnb++(replicate (nfft-nC-1) 0)) nfft fs

  let dat = take anlylen $ drop trlen $ x_hpf
      datlen = length dat
      t = [y/fs |y<-[0.0..((fromIntegral datlen) -1.0)]] :: [Double]
      {- apply whitening filter -}
      whitenedx = take anlylen $ drop nC $map (/sqrt rho) $ fir whnb x

      newpsd = gwpsd whitenedx nfft fs

  print $ findIndices isNaN whitenedx
  print $ findIndices isInfinite whitenedx
  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "after hpf" "149022_afterhpf.png"  $ gwpsd trdat nfft fs
--  HR.plot HR.Linear HR.Line ("time", "amplitude") "after" "whiteningTest.png" $ zip t  whitenedx
--  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "before" "whiteningTestbefore.png" $ psddat --gwpsd whitenedx 16384 16384
  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "after" "149022_whiteningTestbafter.png" newpsd
  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "transfer function" "149022_whiteningtransferFunction.png" psd_whnb



