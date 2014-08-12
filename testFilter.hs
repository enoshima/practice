{-
- test code to check iirFilter and butter in SignalProcessingUtils
- to compile the code, run
- ghc -o testFilter testFilter.hs HasKAL/SignalProcessingUtils/filterFunctions.c
- -}

import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.Chebyshev
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import System.Random

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.PlotUtils.PlotUtilsHROOT

main :: IO ()
main = do
  let x = take 1000 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      (numCoeffLow,denomCoeffLow) = chebyshev1 5 2.0 100 10 Low
      (numCoeffHigh,denomCoeffHigh) = chebyshev1 5 2.0 100 10 High
--      (numCoeffLow,denomCoeffLow) = butter 5 100 10 Low
--      (numCoeffHigh,denomCoeffHigh) = butter 5 100 10 High
-- from matlab
--      numCoeffLow = [0.0296, 0.1775, 0.4438, 0.5918, 0.4438, 0.1775, 0.0296]
--      denomCoeffLow=[1.0, 0.0, 0.7777, 0.0, 0.1142, 0.0, 0.0018]
--      numCoeffHigh= [0.296, -0.1775, 0.4438, -0.5918, 0.4438, -0.1775, 0.0296]
--      denomCoeffHigh=[1.0, 0.0, 0.777, 0.0, 0.1142, 0.0,  0.0018]
--      y = iirFilter x (length x) numCoeffLow denomCoeffLow (length numCoeffLow)
--      z = filtfilt x numCoeffLow denomCoeffLow
--      z = iirFilter x (length x) numCoeffHigh denomCoeffHigh (length numCoeffHigh)
      y = iirFilter x  numCoeffLow denomCoeffLow
--      z = filtfilt x numCoeffLow denomCoeffLow
      z = iirFilter x  numCoeffHigh denomCoeffHigh

  let out1 = gwpsd x 100 100
      out2 = gwpsd y 100 100
      out3 = gwpsd z 100 100

--  print $ take 100 z

  logLogPlot (map fst out1) (map snd out1) "frequency[Hz]" "asd[1/rHz]" Line "beforeFiltered.png"
  logLogPlot (map fst out2) (map snd out2) "frequency[Hz]" "asd[1/rHz]" Line "afterHighPassFiltered.png"
--  logLogPlot (map fst out3) (map snd out3) "frequency[Hz]" "asd[1/rHz]" Line "afterHighPassFiltered.png"
  logLogPlot (map fst out3) (map snd out3) "frequency[Hz]" "asd[1/rHz]" Line "afterHighPassFiltfiltered.png"


  writeFile "1originalData.dat" $ unwords $ map (\xs -> show xs :: String) x
  writeFile "1LowPassFilteredData.dat" $ unwords $ map (\xs -> show xs :: String) y
  writeFile "1HighPassFilteredData.dat" $ unwords $ map (\xs -> show xs :: String) z




