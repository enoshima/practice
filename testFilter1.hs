

import Filter
import System.Random
import qualified Data.Vector.Unboxed as UV
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SpectrumUtils.SpectrumUtils
import qualified Data.Array as AB
import qualified Data.Array.Unboxed as AU
import Data.Time
import HasKAL.SignalProcessingUtils.Filter


main = do
  let x = UV.fromList $ take 102400 $ randomRs (-1, 1) $ mkStdGen 1 :: UV.Vector Double
      fs = 128
      fc = 10
      nfft = truncate fs
      (numC', denomC') = butter 6 fs fc Low
      numC = UV.fromList numC'
      denomC = UV.fromList denomC'
--      numA = AU.array (0, 6) $ zip [0, 1..] numC'
--      denomA = AU.array (0, 6) $ zip [0, 1..] denomC'


  t1 <- getCurrentTime
--  let y2 = DSP.iir_df2 (numA, denomA) (UV.toList x)
--  print $ take 10 y2
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

  t7 <- getCurrentTime
  let y3 = iir_df2 (numC', denomC') (UV.toList x)
  print $ take 10 y3
  t8 <- getCurrentTime
  print $ diffUTCTime t8 t7

  t3 <- getCurrentTime
  let y4 = iirFilter (UV.toList x) numC' denomC'
  print $ take 10 y4
  t4 <- getCurrentTime
  print $ diffUTCTime t4 t3



--  HR.dPlot HR.LogXY HR.Line [("frequency [Hz]", "asd[Hz^-1/2]"), ("frequency [Hz]", "asd[Hz^-1/2]"), ("frequency [Hz]", "asd[Hz^-1/2]")] ["original", "filtered", "dsp_filtered"] "plotPsd.png" $ [gwpsd (UV.toList x) nfft fs, gwpsd y nfft fs, gwpsd y2 nfft fs]
--
--  HR.dPlot HR.LogXY HR.Line [("frequency [Hz]",  "asd[Hz^-1/2]"),  ("frequency [Hz]",  "asd[Hz^-1/2]")] ["original", "dsp_filtered"] "plotPsd.png" $ [gwpsd (UV.toList x) nfft fs,  gwpsd y2 nfft fs]

  HR.dPlot HR.LogXY HR.Line [("frequency [Hz]","asd[Hz^-1/2]"),("frequency [Hz]","asd[Hz^-1/2]"),("frequency [Hz]","asd[Hz^-1/2]")] ["original", "dsp_filtered", "filtered"] "plotPsd.png" $ [gwpsd y3 nfft fs,gwpsd y3 nfft fs, gwpsd y4 nfft fs]





