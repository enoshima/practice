


{-
 - Test of Burst pipeline
 -
 -}


import Control.Monad.ST (ST)
import Data.Packed.ST
import Data.List
import Data.Time
import HasKAL.DetectorUtils.Detector
import HasKAL.FrameUtils.FrameUtils
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon
import HasKAL.MonitorUtils.SRMon.StudentRayleighThreshold
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

      xs = downsample fs' fs $ map realToFrac (eval fdata)
      stopgpss = formatGPS $ deformatGPS startgps + 1/fs*fromIntegral (length xs)
      ligodatas = mkLIGOHanfordWaveData "h-of-t" fs startgps stopgpss $ fromList xs


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
  print "{- Time-Frequency SNR Map -}"
  let noverlap = 0 :: Int
      nfreq = 256 :: Int
      ntime = 140 :: Int
      fs2 = floor $ ((fromIntegral nfreq)/2) :: Int

      nrefset = 50 :: Int
      refpsd = snd $ gwpsdV (subVector 0 (nfreq*nrefset) (gwdata whnWaveData)) nfreq fs
      refpsd2 = scale 2 $ subVector 0 fs2 refpsd

  HR.plot HR.LogXY HR.Line ("frequency",  "Spectrum") "ref psd" "testburst_refpsd.png" ((0, 0), (0, 0))
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
  let nset = floor (fromIntegral (dim (gwdata ligodatas))/(fromIntegral nfreq))
      noff = take nset
        $ map (\x->toList x)
        $ map (\x->sqrt $ snd $ (gwpsdV x nfreq fs))
        $ takesV (replicate nset nfreq) (gwdata ligodatas)
      numF = 1

  let refpsds = snd $ gwpsdV (gwdata ligodatas) nfreq fs
      nu = flip (!!) 0 $ studentRayleighMon' MLE nset 0 numF (toList refpsds) noff
      nufactor = map (\x-> studentThreshold 2.0 x) nu
      snrMatPs = fromColumns
        $ map (\i->zipVectorWith (/)
        (
        subVector 0 fs2 $ snd $ gwpsdV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
        (fromList (zipWith (*) nufactor (toList refpsd)))
        ) [0..ntime-1]
      snrMats = (snrMatT, snrMatF, snrMatPs)
  print nu

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
  let thres4MG = Numeric.LinearAlgebra.find (<4.0) mg
      thres4MS = Numeric.LinearAlgebra.find (<4.0) ms
      mg4 = updateSpectrogramSpec snrMat' $ updateMatrixElement mg thres4MG
      ms4 = updateSpectrogramSpec snrMats' $ updateMatrixElement ms thres4MS
  t18 <- getCurrentTime
  print $ diffUTCTime t18 t17

  print "{- plot SNR-spectrogram -}"
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR Spectrogram") "testburst_snrSpec.png" $ plotFormatedSpectogram snrMat'
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs Spectrogram") "testburst_snrSpec_student.png" $ plotFormatedSpectogram snrMats'

  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNR>5 Spectrogram") "testburst_snrSpec_t4.png" $ plotFormatedSpectogram mg4
  HR3.spectrogram HR3.LogY HR3.COLZ " " ("SNRs>5 Spectrogram") "testburst_snrSpec_student_t4.png" $ plotFormatedSpectogram ms4






plotFormatedSpectogram :: Spectrogram -> [(Double, Double, Double)]
plotFormatedSpectogram (tV, freqV, specgram) = do
  let tV' = concat [ replicate (dim freqV) x | x <- toList tV]
      freqV'=take (dim tV * dim freqV) (cycle $ toList freqV)
  zip3 tV' freqV' (concat $ map (\x->toList x) $ toColumns specgram)


updateMatrixElement :: Matrix Double -> [(Int, Int)] -> Matrix Double
updateMatrixElement s w = runSTMatrix $ do
  s' <- unsafeThawMatrix s
  mapM_ (\x->unsafeWriteMatrix s' (fst x) (snd x) 0.0) w
  return s'


updateSpectrogramSpec :: Spectrogram -> Matrix Double -> Spectrogram
updateSpectrogramSpec s m = (t, f, m)
  where (t, _, _) = s
        (_, f, _) = s






