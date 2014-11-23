
-- fir'0 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
-- fir'0 h w []     = y : []
--     where y  = sum [ h!i * w!i | i <- [0..m] ]
--           m  = snd $ bounds h
-- fir'0 h w (x:xs) = y : fir'0 h w' xs
--     where y  = sum [ h!i * w!i | i <- [0..m] ]
--           w' = listArray (0,m) $ x : elems w
--           m  = snd $ bounds h
--

-- to compilation
-- ghc -threaded -rtsopts -fllvm -L/usr/local/opt/llvm33/lib/llvm-3.3/lib/ -eventlog fir_repa.hs
--
-- to run the code
-- ./fir_repa +RTS -N2
--
-- if you want use ThreadScope,
-- ghc -threaded -rtsopts -fllvm -L/usr/local/opt/llvm33/lib/llvm-3.3/lib/
-- -eventlog
--
-- ./fir_repa +RTS -N2 -l
--
-- Then run
-- % threadscope fir_repa.eventlog
--


import qualified Data.Array.Repa as Repa
import Data.Array.Repa
import qualified Data.Array.Repa.Repr.Unboxed as RRU
--import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
--import qualified Numeric.LinearAlgebra as NL
import System.Random
import HasKAL.SignalProcessingUtils.ButterWorth
import Data.Time

import FIRr
import FIRfft

--main :: IO (Array U DIM1 Double)
main = do

  let datalen = 2^21 :: Int
      vx = VU.fromList $ Prelude.take datalen $ randomRs (-1,1) $ mkStdGen 1 :: VU.Vector Double
      vh = VU.fromList $ snd $ butter 6 1024 50 High
      vw = VU.fromList $ (vx VU.! 0):Prelude.replicate 6 0

      x = RRU.fromUnboxed (Repa.ix1 datalen) vx
      h = RRU.fromUnboxed (Repa.ix1 6) vh
      w = RRU.fromUnboxed (Repa.ix1 7) vw

--  print "aaa"
--  t1 <- getCurrentTime
--  fir0repa h w x
--  t2 <- getCurrentTime
--  print $ diffUTCTime t2 t1
--  t3 <- getCurrentTime
  fftfilt vh vx
--  t4 <- getCurrentTime
--  print $ diffUTCTime t4 t3

