

import Resampling
import System.Random
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR

main = do
  let x = take 10240 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      fs = 1024 :: Double
      newfs = 256 :: Double

      x_d = downsample fs newfs x

  print $ length x
  print $ length x_d
  print $ take 5 x_d
  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "Downsampling Test" "plot_downsampling.png"  $ gwpsd x_d (truncate newfs) newfs



