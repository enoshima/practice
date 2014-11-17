{-
  performance test of repa-iir

to compile
ghc -threaded -rtsopts -fllvm -L/usr/local/opt/llvm33/lib/llvm-3.3/lib/ testIIRr.hs HasKAL/SignalProcessingUtils/filterFunctions.c
./testIIRr +RTS -N2
-}


import qualified Data.Vector.Unboxed as UV
import System.Random
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.Filter

--import IIRr
import qualified IIRstt as UVIIR

import Control.DeepSeq (deepseq)
import Data.Time


main :: IO ()
main = do
  let x = take 10240 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      (b, a) = butter 6 1024 20 High :: ([Double], [Double])
  x `deepseq` return ()
  (b, a) `deepseq` return ()

  -- ^ evaluate C-iir
  t1 <- getCurrentTime
  let ciir = iirFilter x b a
  ciir `deepseq` return ()
  print $ take 10 ciir
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

-- ^ evaluate IIR unboxed vector
  let v = UV.fromList x :: UV.Vector Double
  t3 <- getCurrentTime
  y <- UVIIR.iir (b, a) v
  y `deepseq` return ()
  t4 <- getCurrentTime
  print $ take 10 $ UV.toList y
  print $ diffUTCTime t4 t3




  -- ^ evaluate Repa-iir
--  let v = UV.fromList x :: UV.Vector Double
--  v `deepseq` return ()
--  t3 <- getCurrentTime
--  let iired = iir (b, a) v
--  print $ take 10 $ UV.toList iired
--  t4 <- getCurrentTime
--  print $ diffUTCTime t4 t3

  print "test completed"



