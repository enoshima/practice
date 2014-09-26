

import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.SpectrumUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import Filter
import qualified Data.Vector.Unboxed as UV


main = do
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  [(chname,   _)] <- getChannelList fname
  fs <- getSamplingFrequency fname chname
  let newfs = fs/4

  tmpdat <- readFrame chname fname
  let gwdat = UV.fromList $ map realToFrac (eval tmpdat)

  let (b, a) = butter 6 fs 50 High
      numC = UV.fromList b
      denomC = UV.fromList a

  let hpgwdat = fir (UV.fromList [1..1000::Double]) gwdat

  HR.plot HR.LogXY HR.Line ("frequency [Hz]",   "asd[Hz^-1/2]") "after" "plotPsd_after.png" $ gwpsd (UV.toList hpgwdat) (truncate fs) fs
