
{-
 - unit test of software injection
 -
-}


import HasKAL.DetectorUtils
import HasKAL.FrameUtils.FrameUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import HasKAL.SignalProcessingUtils.Resampling
import HasKAL.SimulationUtils.Injection
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.TimeUtils
import HasKAL.WaveUtils
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
      stopgps = formatGPS $ deformatGPS startgps + 1/fs*fromIntegral (length x)

  {- construct WaveData DataType -}
  let ligodata = mkLIGOWaveData "h-of-t" fs startgps stopgps $ fromList x

--  print $ samplingFrequency ligodata

  {- generate Injection signal -}
  let injhrss = 1.0E-16
      srctype = mkSOURCE_TYPE_S5 "dfmr_A1B1G1.txt" 0.0 0.0 0.0 injhrss
      injgps = (877201788, 0) :: GPSTIME
      injdetresp = downsampleWaveData fs
                 $ injDetectorResponse LIGO_Hanford srctype injgps :: WaveData

  {- do injection -}
  let injected = doInjection ligodata injdetresp

  {- plot resutls -}
  HR.plot HR.LogXY HR.Line ("frequency",  "Spectrum") "Injection Test" "plot_injection.png" $ gwpsd (toList (subVector 0 (4*4096) (gwdata injected))) (truncate fs) fs
  HR.plot HR.Linear HR.Line ("time",  "h") "Injection Test" "plot_injection_ts.png" $ zip [1..] (toList (gwdata injdetresp))



