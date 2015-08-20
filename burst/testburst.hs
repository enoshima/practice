{-# LANGUAGE BangPatterns #-}


{-
 - Test of Burst pipeline
 - to compile
 ghc testburst.hs HasKAL/PlotUtils/HROOT/AppendFunction.cc HasKAL/SignalProcessingUtils/filterFunctions.c -lFrame -L$HOME/haskell/core/libframe/lib/ -L/usr/local/lib/root -I/usr/local/include/root/
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
import qualified Numeric.LinearAlgebra as NL
import Numeric.GSL.Fourier
import System.Random (randomIO)
import System.IO.Unsafe (unsafePerformIO)

import Data.Time
--import System.IO.Unsafe (unsafePerformIO)
import PipelineFunction
import FFTW

main = do

  {- setting search parameters -}
  let sigma = 2.0 :: Double

  t1 <- getCurrentTime
  print "{- input data information -}"
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  let chname = "H2:LSC-STRAIN"
--  Just [(fdata, fs', startgps, _)] <- dataInfo "cache.file" "H2:LSC-STRAIN"
  Just (fdata, fs', startgps, dt) <- frameInfo fname "H2:LSC-STRAIN"

  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1


  t3 <- getCurrentTime
  print "{- downsampling -}"
  let fs = 4096 :: Double
      x = G.take (truncate (fs*10)) $ downsampleV fs' fs fdata
      stopgps = formatGPS $ deformatGPS startgps + 1/fs*fromIntegral (G.length x)
      {- construct WaveData DataType -}
      ligodata = mkLIGOHanfordWaveData "h-of-t" fs startgps stopgps x
--  print $ take 5 $ toList fdata

  let xs = downsampleV fs' fs fdata
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
  let !injected = doInjection' ligodata injdetresp
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
  let !whnWaveData = dropWaveData (2*nC) $ whiteningWaveData whnParam injected
  t12 <- getCurrentTime
  print $ diffUTCTime t12 t11


  HR.plot HR.Linear HR.Line 1 HR.RED ("time", "amplitude") 0.05 "whitened" "testburst_conditioned.png" ((0, 0), (0, 0)) $ zip [0, 1..] $ toList (gwdata whnWaveData)
  HR.plot HR.LogXY HR.Line 1 HR.RED ("frequency", "Spectrum") 0.05 "whitened psd" "testburst_conditioned_psd.png" ((0, 0), (0, 0)) $ gwpsd (toList (gwdata whnWaveData)) nfft fs

  t13 <- getCurrentTime
  print "{- Time-Frequency SNR Map -}"
  let noverlap = 0 :: Int
      nfreq = 256 :: Int -- 256
      ntime = 140 :: Int -- 140
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
      snrMatP' = (fs2><ntime) $ concat $
        map (\i -> map (\x-> x!!i) $ map (\i->toList $ zipVectorWith (/)
        (
        subVector 0 fs2 $ snd $ gwpsdV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
        refpsd2
        ) [0..ntime-1]) [0..fs2-1] :: Matrix Double
  t14 <- getCurrentTime
  print $ diffUTCTime t14 t13

  tx1 <- getCurrentTime
  let dcted' = dct2d snrMatP'
      ncol = cols dcted'
      nrow = rows dcted'
      zeroElementc = [(x, y) | x<-[0..nrow-1], y<-[ncol-20..ncol-1]]
      zeroElementr = [(x, y) | y<-[0..ncol-1], x<-[nrow-80..nrow-1]]
      zeroElement = zeroElementr ++ zeroElementc
      dcted = updateMatrixElement dcted' zeroElement $ take (length zeroElement) [0, 0..]
      snrMatP = idct2d dcted
  tx2 <- getCurrentTime
  print $ diffUTCTime tx2 tx1


  t15 <- getCurrentTime
  print "{- remove low frequency region from the spectrogram. -}"
  {- find first index of which an element satisfy a given condition -}
  let cutoffF = 64.0
      thresIndex = head $ Numeric.LinearAlgebra.find (>=cutoffF) snrMatF
--      snrMatDCT = (snrMatT, snrMatF, dcted')
      !snrMat' = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatP')
      !snrMat = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatP)
  t16 <- getCurrentTime
  print $ diffUTCTime t16 t15


  t17 <- getCurrentTime
  print "{- thresholding -}"
  let (_, _, mg') = snrMat'
      normalizemg't = scale (1/(mg' @@> (maxIndex mg'))) mg'
  let (_, _, mg) = snrMat
      normalizemgt = scale (1/(mg @@> (maxIndex mg))) mg
  let thresNMG' = Numeric.LinearAlgebra.find (<0.25) normalizemg't
  let thresNMG = Numeric.LinearAlgebra.find (<0.25) normalizemgt
      normalizemg' = updateSpectrogramSpec snrMat' $ updateMatrixElement normalizemg't thresNMG' (replicate (length thresNMG') 0.0)
      normalizemg = updateSpectrogramSpec snrMat $ updateMatrixElement normalizemgt thresNMG (replicate (length thresNMG) 0.0)
  let thres4MG' = Numeric.LinearAlgebra.find (<3.0) mg'
      thres4MG = Numeric.LinearAlgebra.find (<3.0) mg
      mg4' = updateSpectrogramSpec snrMat' $ updateMatrixElement mg' thres4MG' (replicate (length thres4MG') 0.0)
      mg4 = updateSpectrogramSpec snrMat $ updateMatrixElement mg thres4MG (replicate (length thres4MG) 0.0)

  t18 <- getCurrentTime
  print $ diffUTCTime t18 t17

  print "{- plot SNR-spectrogram -}"
--  HR3.spectrogram HR3.LogZ HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec_dcted.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram snrMatDCT
  HR3.spectrogram HR3.LogYZ HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram snrMat'
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR>5 Spectrogram") "testburst_snrSpec_t3_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram mg4'
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec_norm_thr025_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram normalizemg'


  HR3.spectrogram HR3.LogYZ HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec_sigma2_New.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram snrMat
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR>5 Spectrogram") "testburst_snrSpec_t3_sigma2_New.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram mg4
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec_norm_thr025_sigma2_New.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram normalizemg

--  t18' <- getCurrentTime
--  print $ diffUTCTime t18' t18


--   t19 <- getCurrentTime
--   print "{- Time-Frequency SNR Map using Student-t noise modeling -}"
--   let stride = 256
--       hfs = gwspectrogramV 0 stride fs (gwdata ligodatas)
--       clusteringN = 1
--       shiftN = 20
--       chunkN = 20
--       aveN = 20
--
--   let refpsds = gwpsdV (subVector 0 (stride*aveN) (gwdata ligodatas)) stride fs
--       (_, _, nuMatrix) = studentRayleighMonV MLE fs stride chunkN shiftN clusteringN refpsds hfs
--       nu = flip (!!) 0 $ toColumns nuMatrix
--       nufactor = fromList $ map (\x-> 1/sigma*studentThreshold sigma x) $ toList nu
--       snrMatPs = fromColumns
--         $ map (\i->zipVectorWith (/)
--         (
--         subVector 0 fs2 $ snd $ gwpsdV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
--         (zipVectorWith (*) nufactor refpsd2)
--         ) [0..ntime-1]
--       snrMats = (snrMatT, snrMatF, snrMatPs)
--
--   HR.plot HR.Linear HR.Line 1 HR.RED ("frequency [Hz]",  "nu factor") 0.05 " " "testburst_nu.png" ((0, 0), (0, 0))
--     $ zip [0, 16..2048]  (toList nufactor)
--   t20 <- getCurrentTime
--   print $ diffUTCTime t20 t19
--
--
--   t21 <- getCurrentTime
--   print "{- remove low frequency region from the spectrogram. -}"
--   {- find first index of which an element satisfy a given condition -}
--   let cutoffF = 64.0
--       thresIndex = head $ Numeric.LinearAlgebra.find (>=cutoffF) snrMatF
--       snrMats' = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatPs)
--   t22 <- getCurrentTime
--   print $ diffUTCTime t22 t21
--
--
--   t23 <- getCurrentTime
--   let (_, _, ms) = snrMats'
--       normalizems' = scale (1/(ms @@> (maxIndex ms))) ms
--
--   let thresNMS = Numeric.LinearAlgebra.find (<0.25) normalizems'
--       normalizems = updateSpectrogramSpec snrMats' $ updateMatrixElement normalizems' thresNMS (replicate (length thresNMS) 0.0)
--   t18 <- getCurrentTime
--   print $ diffUTCTime t18 t17
--
--
--   let thres4MS = Numeric.LinearAlgebra.find (<3.0) ms
--       ms4 = updateSpectrogramSpec snrMats' $ updateMatrixElement ms thres4MS (replicate (length thres4MS) 0.0)
--   t24 <- getCurrentTime
--   print $ diffUTCTime t24 t23
--
--   print "{- plot SNR-spectrogram -}"
--   HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs Spectrogram") "testburst_snrSpec_student_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram snrMats'
--
--   HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs>5 Spectrogram") "testburst_snrSpec_student_t3_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram ms4
--
--   HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs Spectrogram") "testburst_snrSpec_nrm_student_thr025_sigma2.png" ((0, 0), (0, 0)) $ plotFormatedSpectogram normalizems
--


disp = putStr . dispf 2


--randnM r c = unsafePerformIO $ do
--  seed <- randomIO
--  return (NL.reshape c $ NL.randomVector seed NL.Gaussian (r*c))

--
