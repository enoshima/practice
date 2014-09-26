
import HasKAL.SignalProcessingUtils.Filter
import qualified Data.Vector.Unboxed as UV
import System.Random
import Data.Time
import Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType
import qualified Numeric.Signal as S
import qualified Numeric.LinearAlgebra as L

main = do
  let (zeroC, poleC) = butter 6 1024 500 High
      x = take 10240 $ randomRs (-10, 10) $ mkStdGen 10 :: [Double]

  x1 <- getCurrentTime
  print $ take 5 $ iirFilter x zeroC poleC
  y1 <- getCurrentTime
  print $ diffUTCTime y1 x1

--  x2 <- getCurrentTime
--  print $ take 5 $ UV.toList $ UV.map (\i-> sum [ (UV.fromList fcoef)UV.!j * (UV.fromList x)UV.!(i-1-j) | j<-[0..9]]) (UV.fromList [10..100])
--  y2 <- getCurrentTime
--  print $ diffUTCTime y2 x2

--  x2 <- getCurrentTime
--  print $ take 5 $ L.toList $ S.filter (L.fromList zeroC) (L.fromList poleC) 1024 (L.fromList x)
--  y2 <-getCurrentTime
--  print $ diffUTCTime y2 x2

  x3 <- getCurrentTime
  print $ take 5 $ UV.toList $ iir_df2 (UV.fromList zeroC) (UV.fromList poleC) (UV.fromList x)
  y3 <- getCurrentTime
  print $ diffUTCTime y3 x3

