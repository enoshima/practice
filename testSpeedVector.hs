


--module Main where
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import Data.Time
import System.Random

main = do
  let x = take 10000000 $ randomRs (-10, 10) $ mkStdGen 1 :: [Double]

  t1 <- getCurrentTime
  -- for Data.Vector.Unboxed
  print $ UV.head $ UV.reverse $ UV.fromList x
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

  t3 <- getCurrentTime
  -- for Data.Vector.Storable
  print $ SV.head $ SV.reverse $ SV.fromList x
  t4 <- getCurrentTime
  print $ diffUTCTime t4 t3



