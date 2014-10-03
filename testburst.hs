


{-
 - Test of Burst pipeline
 -
 -}



import Data.List
import Data.Time
import HasKAL.DetectorUtils.Detector
import HasKAL.FrameUtils.FrameUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as HR3
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

import Data.Time


main = do
  t1 <- getCurrentTime
  print "{- input data information -}"
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  [(chname,  _)] <- getChannelList fname
  fdata <- readFrame chname fname
  fs' <- getSamplingFrequency fname chname
  startgps <- getGPSTime fname
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

  t3 <- getCurrentTime
  print "{- downsampling -}"
  let fs = 4096 :: Double
      x = take (truncate (fs*10)) $ downsample fs' fs $ map realToFrac (eval fdata)
      stopgps = formatGPS $ deformatGPS startgps + 1/fs*fromIntegral (length x)
      {- construct WaveData DataType -}
      ligodata = mkLIGOHanfordWaveData "h-of-t" fs startgps stopgps $ fromList x

  print $ take 5 $ toList (gwdata ligodata)
  t4 <- getCurrentTime
  print $ diffUTCTime t4 t3


  t5 <- getCurrentTime
  print "{- generate Injection signal -}"
  let injhrss = 1.0E-18
      srctype = mkSOURCE_TYPE_S5 "dfmr_A1B1G1.txt" 0.0 0.0 0.0 injhrss
      injgps = (877201793, 0) :: GPSTIME
      injdetresp = downsampleWaveData fs
                 $ injDetectorResponse LIGO_Hanford srctype injgps :: WaveData

  print $ take 5 $ toList (gwdata injdetresp)
  t6 <- getCurrentTime
  print $ diffUTCTime t6 t5

  t7 <- getCurrentTime
  print "{- do injection -}"
  let injected = doInjection' ligodata injdetresp
  HR.plot HR.Linear HR.Line ("time",  "amplitude") "injected" "testburst_injected.png" ((0, 0), (0, 0)) $ zip [0, 1..] $ toList (gwdata injected)

  t8 <- getCurrentTime
  print $ diffUTCTime t8 t7

  {- whitening filter properties -}
  let nfft = truncate fs
      nC = 2048 - 1

  t9 <- getCurrentTime
  print "{- whitening filter coefficients -}"
  let trlen = truncate fs
      trdat = take trlen $ toList $ gwdata injected
      whnParam = lpefCoeff nC (gwpsd trdat nfft fs)

  print (snd whnParam)
  t10 <- getCurrentTime
  print $ diffUTCTime t10 t9

  t11 <- getCurrentTime
  print "{- apply whitening filter -}"
  let whnWaveData = dropWaveData (2*nC) $ whiteningWaveData whnParam injected

  HR.plot HR.Linear HR.Line ("time",  "amplitude") "whitened" "testburst_conditioned.png" ((0, 0), (0, 0)) $ zip [0, 1..] $ toList (gwdata whnWaveData)
  HR.plot HR.LogXY HR.Line ("frequency",  "Spectrum") "whitened psd" "testburst_conditioned_psd.png" ((0, 0), (0, 0)) $ gwpsd (toList (gwdata whnWaveData)) nfft fs

  t12 <- getCurrentTime
  print $ diffUTCTime t12 t11


  t13 <- getCurrentTime
  print "{- Time-Frequency Map -}"
  let noverlap = 0 :: Int
      nfreq = 256 :: Int
      ntime = 140 :: Int
      fs2 = floor $ ((fromIntegral nfreq)/2) :: Int

      nrefset = 100 :: Int
      refpsd = subVector 0 fs2 $ snd $ gwpsdV (subVector 0 (nfreq*nrefset) (gwdata whnWaveData)) nfreq fs
  HR.plot HR.LogXY HR.Line ("frequency",  "Spectrum") "ref psd" "testburst_refpsd.png" ((0, 0), (0, 0))
    $ zip [0..] (toList refpsd)
    -- todo : functionalization
  let snrMatF = scale (fs/fromIntegral nfreq) $ linspace nfreq (0, fromIntegral nfreq)
      snrMatT = scale ((fromIntegral nfreq)/fs) $ fromList [0.0, 1.0..(fromIntegral ntime -1)]
      snrMatP = fromColumns
        $ map (\i->zipVectorWith (/)
        (subVector 0 fs2 $ snd $ gwpsdV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs) refpsd)
        [0..ntime-1]
      snrMat = (snrMatT, snrMatF, snrMatP)

      nrow = rows snrMatP
      ncol = cols snrMatP

  t14 <- getCurrentTime
  print $ diffUTCTime t14 t13

  t15 <- getCurrentTime
  print "{- remove low frequency region from the spectrogram. -}"
  {- find first index of which an element satisfy a given condition -}
  let cutoffF = 64.0
      thresIndex = head $ Numeric.LinearAlgebra.find (>=cutoffF) snrMatF
      snrMat' = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatP)
  t16 <- getCurrentTime
  let (_, _, checkingM) = snrMat'
  print $ checkingM @@> (1, 1)
  print $ diffUTCTime t16 t15

  print "{- plot SNR-spectrogram -}"
  HR3.spectrogram HR3.Linear HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec.png" $ plotFormatedSpectogram snrMat'
--
--
--
--
--
--
plotFormatedSpectogram :: Spectrogram -> [(Double, Double, Double)]
plotFormatedSpectogram (tV, freqV, specgram) = do
  let tV' = concat [ replicate (dim freqV) x | x <- toList tV]
      freqV'=take (dim tV * dim freqV) (cycle $ toList freqV)
  zip3 tV' freqV' (concat $ map (\x->toList x) $ toColumns specgram)









