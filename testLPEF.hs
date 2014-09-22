{-
 - unit test of whitening filter based on linear prediction error filter
 - for compilation
 - ghc--make testLPEF.hs
 -}

import Data.List
import Data.Time
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import HasKAL.SignalProcessingUtils.ButterWorth
--import HasKAL.SignalProcessingUtils.Chebyshev
--import HasKAL.SignalProcessingUtils.Filter
import qualified HasKAL.SignalProcessingUtils.FilterH as FH
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.LinearPrediction
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.FrameUtils.FrameUtils
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Resampling


main = do
  t1 <- getCurrentTime
  {- input data information -}
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname,  _)]=ch
  fdata <- readFrame chname fname
  fs' <- getSamplingFrequency fname chname
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

  {- downsampling -}
  t3 <- getCurrentTime
  let fs = 4096 :: Double
      x = downsample fs' fs $ map realToFrac (eval fdata)
  t4 <- getCurrentTime
  print $ diffUTCTime t4 t3
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


  t5 <- getCurrentTime
  let anlylen = (truncate fs) * 20
      trlen = (truncate fs)

      {- set training data -}
      trdat = take trlen x_hpf
      {- whitening filter coefficients -}
      (whnb,rho)= lpefCoeff nC (gwpsd trdat nfft fs)
--      whnb = map (fs*) whnb'
      {- transfer function of whitening filter -}
      psd_whnb = gwpsd (whnb++(replicate (nfft-nC-1) 0)) nfft fs
  t6 <- getCurrentTime
  print $ diffUTCTime t6 t5
  
  t7 <- getCurrentTime
  let dat = take anlylen $ drop trlen $ x_hpf
      datlen = length dat
--      t = [y/fs |y<-[0.0..((fromIntegral datlen) -1.0)]] :: [Double]
      {- apply whitening filter -}
      whitenedx = take anlylen $ drop nC $map (/sqrt rho) $ FH.fir whnb x
	  

--      newpsd = gwpsd whitenedx nfft fs
  t8 <- getCurrentTime
  print $ diffUTCTime t8 t7

  t9 <- getCurrentTime
--  print $ findIndices isNaN whitenedx
--  print $ findIndices isInfinite whitenedx
--  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "after hpf" "149022_afterhpf.png"  $ gwpsd trdat nfft fs
--  HR.plot HR.Linear HR.Line ("time", "amplitude") "after" "whiteningTest.png" $ zip t  whitenedx
--  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "before" "whiteningTestbefore.png" $ psddat --gwpsd whitenedx 16384 16384
--  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "after" "149022_whiteningTestbafter.png" newpsd
--  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "transfer function" "149022_whiteningtransferFunction.png" psd_whnb
  print $ take 5 whitenedx
  t10 <- getCurrentTime
  print $ diffUTCTime t10 t9

  t11 <- getCurrentTime
  let whitenedx2 = take anlylen $ drop nC $ map (/sqrt rho) $ firFilter x whnb  
  print $ take 5 whitenedx2
  t12 <- getCurrentTime
  print $ diffUTCTime t12 t11
  
  
  

