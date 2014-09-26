
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import qualified Data.Vector.Unboxed as UV
import System.Random
import Data.Complex

import Data.Time

main = do
  let a = take 10000000000 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]

  x1 <- getCurrentTime
  return $ a ++ reverse a
  y1 <- getCurrentTime
  print $ diffUTCTime y1 x1

  x2 <- getCurrentTime
  return $ UV.toList $ (UV.fromList a) UV.++ UV.reverse (UV.fromList a)
  y2 <- getCurrentTime
  print $ diffUTCTime y2 x2

  x3 <- getCurrentTime
  return $ fft $ fromList [x:+0|x<-a]
  y3 <- getCurrentTime
  print $ diffUTCTime y3 x3

