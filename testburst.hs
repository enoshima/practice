


{-
 - Test of Burst pipeline
 -
 -}



import Data.List
import Data.Time
import HasKAL.FrameUtils.FrameUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import HasKAL.SignalProcessingUtils.ButterWorth
import qualified HasKAL.SignalProcessingUtils.FilterH as FH
import HasKAL.SignalProcessingUtils.LinearPrediction
import HasKAL.SignalProcessingUtils.Resampling
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SimulationUtils.Injection
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.FrameUtils.FrameUtils
import HasKAL.TimeUtils
import HasKAL.WaveUtils
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier


main = do
  {- input data information -}
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname,  _)]=ch
  fdata <- readFrame chname fname
  fs' <- getSamplingFrequency fname chname
  startgps <- getGPSTime fname

  {- downsampling -}
  let fs = 4096 :: Double
      x = downsample fs' fs $ map realToFrac (eval fdata)
      stopgps = formatGPS $ deformatGPS startgps + 1/fs*fromIntegral (length x)

  {- construct WaveData DataType -}
  let ligodata = mkLIGOHanfordWaveData "h-of-t" fs startgps stopgps $ fromList x

  {- generate Injection signal -}
  let injhrss = 1.0E-18
      srctype = mkSOURCE_TYPE_S5 "dfmr_A1B1G1.txt" 0.0 0.0 0.0 injhrss
      injgps = (877201800, 0) :: GPSTIME
      injdetresp = downsampleWaveData fs
                 $ injDetectorResponse LIGO_Hanford srctype injgps :: WaveData

  {- do injection -}
  let injected = doInjection' ligodata injdetresp

  {- whitening filter properties -}
  let nfft = truncate fs
      nC = 2048 - 1

  {- whitening filter coefficients -}
  let trlen = truncate fs
      trdat = take trlen $ toList $ gwdata injected
      whnParam = lpefCoeff nC (gwpsd trdat nfft fs)

  {- apply whitening filter -}
  let whnWaveData = whiteningWaveData whnParam injected

  {- Time-Frequency Map -}
  let noverlap = 0 :: Int
      nf = 256 :: Int
      nrefset = 10 :: Int
      refpsd = snd.unzip.gwpsd nf fs $ toList $ subVector 0 (nf*nrefset) (gwdata whnWaveData)
      snr = map (\i->zipWith (/) (snd.unzip.gwpsd nfft fs $ toList $ subVector (nf*i) nf (gwdata whnWaveData)) refpsd) [0..10]






genTFData :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
genTFData tV freqV spec = do
  let tV' = concat [ replicate (length freqV) x | x <- tV]
      freqV'=take (length tV * length freqV) $ cycle freqV
  zip3 tV' freqV' (concat spec)


