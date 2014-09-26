
{-
 - unit test of software injection
 -
-}


import HasKAL.DetectorUtils
import HasKAL.FrameUtils.FrameUtils
import HasKAL.SimulationUtils.Injection
import HasKAL.TimeUtils.Function
import HasKAL.TimeUtils.Signature
import HasKAL.WaveUtils.Data
import Resampling
import Numeric.LinearAlgebra


main = do
  {- load LIGO data -}
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname, _)] = ch
  fdata <- readFrame chname fname
  fs' <- getSamplingFrequency fname chname
  startgps <- getGPSTime fname

  {- downsampling -}
  let fs = 4096 :: Double
      x = downsample fs' fs $ map realToFrac (eval fdata)
      stopgps = formatGPS $ fromGPS startgps + 1/fs*fromIntegral (length x)

  {- construct WaveData DataType -}
  let ligodata = mkLIGOWaveData "h-of-t" fs startgps stopgps $ fromList x

  print $ samplingFrequency ligodata

  {- generate Injection signal -}
  let injhrss = 1.0E-18
      srctype = mkSOURCE_TYPE_S5 "dfmr_A1B1G1.txt" 0.0 0.0 0.0 injhrss
      injgps = (877201786, 0) :: GPSTIME
      injdetresp = downsampleWaveData fs $ injDetectorResponse LIGO_Hanford srctype injgps :: WaveData

  {- do injection -}
  let injected = doInjection ligodata injdetresp

  return()


