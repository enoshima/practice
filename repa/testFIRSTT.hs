{-
- to compile
- ghc -O2 testFIRSTT.hs HasKAL/SignalProcessingUtils/filterFunctions.c
-}

import qualified HasKAL.SignalProcessingUtils.Filter as C
import qualified FilterSTT as STT
import qualified Data.Vector.Unboxed as UV
import System.Random
import Data.Time
import Control.DeepSeq (deepseq)


main :: IO()
main = do
  let x = take 10 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      v = UV.fromList x
      b = take 3 $ randomRs (-1, 1) $ mkStdGen 2 :: [Double]
  x `deepseq` return ()
  v `deepseq` return ()
  b `deepseq` return ()

  print $ take 5 x
  print $ take 5 b

  t0 <- getCurrentTime
  let cfiltered = C.firFilter x b
  cfiltered `deepseq` return ()
  t1 <- getCurrentTime
  print $ diffUTCTime t1 t0
  print $ take 10 cfiltered

  t2 <- getCurrentTime
  sttfiltered <- STT.fir b v
  sttfiltered `deepseq` return ()
  t3 <- getCurrentTime
  print $ diffUTCTime t3 t2
  print $ take 10 $ UV.toList sttfiltered




