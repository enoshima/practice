


{-
 - Test of Burst pipeline
 -
 -}

import Control.Monad ((>>=))
import Data.Time
import qualified Data.Vector.Generic as G
import HasKAL.DetectorUtils.Detector
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon
import HasKAL.MonitorUtils.SRMon.StudentRayleighThreshold
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as HR3
import HasKAL.SignalProcessingUtils.ButterWorth (butter)
import qualified HasKAL.SignalProcessingUtils.Filter as SP
import HasKAL.SignalProcessingUtils.LinearPrediction (whiteningWaveData, lpefCoeffV)
import HasKAL.SignalProcessingUtils.Resampling (downsampleV, downsampleWaveData)
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SimulationUtils.Injection
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.TimeUtils
import HasKAL.WaveUtils
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier

import Data.Time
--import System.IO.Unsafe (unsafePerformIO)
import PipelineFunction


main = do

  {- setting search parameters -}
  let sigma = 2.0 :: Double

  t1 <- getCurrentTime
  print "{- input data information -}"
--  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  Just (chname, fdata, fs', startgps, _) <- dataInfo "cache.file"
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1


  t3 <- getCurrentTime
  print "{- downsampling -}"
  let fs = 4096 :: Double
      x = G.take (truncate (fs*10)) $ downsampleV fs' fs fdata
      stopgps = formatGPS $ deformatGPS startgps + 1/fs*fromIntegral (G.length x)
      {- construct WaveData DataType -}
      ligodata = mkLIGOHanfordWaveData "h-of-t" fs startgps stopgps x

      xs = downsampleV fs' fs fdata
      stopgpss = formatGPS $ deformatGPS startgps + 1/fs*fromIntegral (G.length xs)
      ligodatas = mkLIGOHanfordWaveData "h-of-t" fs startgps stopgpss xs


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
  HR.plot HR.Linear HR.Line 1 HR.RED ("time",  "amplitude") 0.05 "injected" "testburst_injected.png" ((0, 0), (0, 0)) $ zip [0, 1..] $ toList (gwdata injected)

  t8 <- getCurrentTime
  print $ diffUTCTime t8 t7

  {- whitening filter properties -}
  let nfft = truncate fs
      nC = 2048 - 1


  t9 <- getCurrentTime
  print "{- whitening filter coefficients -}"
  let trlen = truncate fs
      trdat = G.take trlen $ gwdata injected
      whnParam = lpefCoeffV nC (gwpsdV trdat nfft fs)

  print (snd whnParam)
  t10 <- getCurrentTime
  print $ diffUTCTime t10 t9

  t11 <- getCurrentTime
  print "{- apply whitening filter -}"
  let whnWaveData = dropWaveData (2*nC) $ whiteningWaveData whnParam injected

  HR.plot HR.Linear HR.Line 1 HR.RED ("time", "amplitude") 0.05 "whitened" "testburst_conditioned.png" ((0, 0), (0, 0)) $ zip [0, 1..] $ toList (gwdata whnWaveData)
  HR.plot HR.LogXY HR.Line 1 HR.RED ("frequency", "Spectrum") 0.05 "whitened psd" "testburst_conditioned_psd.png" ((0, 0), (0, 0)) $ gwpsd (toList (gwdata whnWaveData)) nfft fs
  t12 <- getCurrentTime
  print $ diffUTCTime t12 t11


  t13 <- getCurrentTime
  print "{- Time-Frequency SNR Map -}"
  let noverlap = 0 :: Int
      nfreq = 256 :: Int
      ntime = 140 :: Int
      fs2 = floor $ ((fromIntegral nfreq)/2) :: Int

      nrefset = 100 :: Int
      refpsd = snd $ gwpsdV (subVector 0 (nfreq*nrefset) (gwdata whnWaveData)) nfreq fs
      refpsd2 = scale sigma $ subVector 0 fs2 refpsd
      refpsd2s= subVector 0 fs2 refpsd

  HR.plot HR.LogXY HR.Line 1 HR.RED ("frequency",  "Spectrum") 0.05 "ref psd" "testburst_refpsd.png" ((0, 0), (0, 0))
    $ zip [0..] (toList refpsd2)
    -- todo : functionalization
  let snrMatF = scale (fs/fromIntegral nfreq) $ linspace nfreq (0, fromIntegral nfreq)
      snrMatT = scale ((fromIntegral nfreq)/fs) $ fromList [0.0, 1.0..(fromIntegral ntime -1)]
      snrMatP = fromColumns
        $ map (\i->zipVectorWith (/)
        (
        subVector 0 fs2 $ snd $ gwpsdV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
        refpsd2
        ) [0..ntime-1]
      snrMat = (snrMatT, snrMatF, snrMatP)
      nrow = rows snrMatP
      ncol = cols snrMatP
  t14 <- getCurrentTime
  print $ diffUTCTime t14 t13


  t15 <- getCurrentTime
  print "{- Time-Frequency SNR Map using Student-t noise modeling -}"
  let stride = 256
      hfs = gwspectrogramV 0 stride fs (gwdata ligodatas)
      clusteringN = 1
      shiftN = 20
      chunkN = 20
      aveN = 20

  let refpsds = gwpsdV (subVector 0 (stride*aveN) (gwdata ligodatas)) stride fs
      (_, _, nuMatrix) = studentRayleighMonV MLE fs stride chunkN shiftN clusteringN refpsds hfs
      nu = flip (!!) 0 $ toColumns nuMatrix
      nufactor = fromList $ map (\x-> 1/sigma*studentThreshold sigma x) $ toList nu
      snrMatPs = fromColumns
        $ map (\i->zipVectorWith (/)
        (
        subVector 0 fs2 $ snd $ gwpsdV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
        (zipVectorWith (*) nufactor refpsd2)
        ) [0..ntime-1]
      snrMats = (snrMatT, snrMatF, snrMatPs)

  HR.plot HR.Linear HR.Line 1 HR.RED ("frequency [Hz]",  "nu factor") 0.05 " " "testburst_nu.png" ((0, 0), (0, 0))
    $ zip [0, 16..2048]  (toList nufactor)


  t15 <- getCurrentTime
  print "{- remove low frequency region from the spectrogram. -}"
  {- find first index of which an element satisfy a given condition -}
  let cutoffF = 64.0
      thresIndex = head $ Numeric.LinearAlgebra.find (>=cutoffF) snrMatF
      snrMat' = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatP)
      snrMats' = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatPs)
  t16 <- getCurrentTime
  print $ diffUTCTime t16 t15


  t17 <- getCurrentTime
  let (_, _, mg) = snrMat'
      (_, _, ms) = snrMats'

      normalizemg' = scale (1/(mg @@> (maxIndex mg))) mg
      normalizems' = scale (1/(ms @@> (maxIndex ms))) ms

  let thresNMG = Numeric.LinearAlgebra.find (<0.25) normalizemg'
      thresNMS = Numeric.LinearAlgebra.find (<0.25) normalizems'
      normalizemg = updateSpectrogramSpec snrMat' $ updateMatrixElement normalizemg' thresNMG (replicate (length thresNMG) 0.0)
      normalizems = updateSpectrogramSpec snrMats' $ updateMatrixElement normalizems' thresNMS (replicate (length thresNMS) 0.0)
  t18 <- getCurrentTime
  print $ diffUTCTime t18 t17


  let thres4MG = Numeric.LinearAlgebra.find (<3.0) mg
      thres4MS = Numeric.LinearAlgebra.find (<3.0) ms
      mg4 = updateSpectrogramSpec snrMat' $ updateMatrixElement mg thres4MG (replicate (length thres4MG) 0.0)
      ms4 = updateSpectrogramSpec snrMats' $ updateMatrixElement ms thres4MS (replicate (length thres4MS) 0.0)
  t18 <- getCurrentTime
  print $ diffUTCTime t18 t17

  print "{- plot SNR-spectrogram -}"
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram snrMat'
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs Spectrogram") "testburst_snrSpec_student_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram snrMats'

  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR>5 Spectrogram") "testburst_snrSpec_t3_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram mg4
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs>5 Spectrogram") "testburst_snrSpec_student_t3_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram ms4

  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec_norm_thr025_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram normalizemg
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs Spectrogram") "testburst_snrSpec_nrm_student_thr025_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram normalizems




