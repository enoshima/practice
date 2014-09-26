
{-
 - developing code to do software injection
 -}


import HasKAL.DetectorUtils.Detector
import HasKAL.DetectorUtils.DetectorParam
import HasKAL.DetectorUtils.Functions
import Numeric.LinearAlgebra
import HasKAL.PlotUtils.HROOT.PlotGraph
import System.IO.Unsafe

main :: IO()
main = do
  let detname = LIGO_Hanford
      detparam = ligoHanford
      phiS = 0 :: Double
      thetaS=0 :: Double
      psiS = 0 :: Double
      (fplusS, fcrossS, tauS) = fplusfcrossts detparam phiS thetaS psiS
      hplus = fromList $ map (\x -> read x :: Double) (lines $ unsafePerformIO $ readFile "SG235Q8d9.txt")
      detresp = (scalar fplusS) * hplus
      tV = [0, 1/16384..]
  plot Linear Line ("time[s]", "amplitude") "LIGO Hanford detector response" "Hdetresp_InjSG235Q8d9.eps" $ zip tV (toList detresp)
