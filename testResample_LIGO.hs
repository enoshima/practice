

import Resampling
import System.Random
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.FrameUtils.FrameUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR

main = do

  {- input data information -}
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname,  _)]=ch
  fdata <- readFrame chname fname
  fs' <- getSamplingFrequency fname chname

  let x = take (10*truncate fs') $ map realToFrac (eval fdata)
      fs = 4096 :: Double

      x_d = downsample fs' fs x

  print $ length x
  print $ length x_d
  print $ take 5 x_d
  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "Downsampling Test" "plot_downsampling4096wAAF_LIGO.png"  $ gwpsd x_d (truncate fs) fs



