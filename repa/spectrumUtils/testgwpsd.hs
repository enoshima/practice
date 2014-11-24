

{-
- ghc -threaded -rtsopts -fllvm -L/usr/local/opt/llvm33/lib/llvm-3.3/lib/
- -L~/haskell/core/libframe/lib -lFrame -I/usr/local/include/root
- testgwpsd.hs HasKAL/PlotUtils/HROOT/AppendFunction.cc
- ./testgwpsd +RTS -N2
-
- -}

import Function
import WindowFunction
import WindowType
import qualified Data.Vector.Unboxed as UV
import HasKAL.FrameUtils.FrameUtils
import HasKAL.SignalProcessingUtils.Resampling
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR

import Data.Time

main = do
  print "{- input data information -}"
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  [(chname, _)] <- getChannelList fname
  fdata <- readFrame chname fname
  fs' <- getSamplingFrequency fname chname


  print "{- downsampling -}"
  let  fs = 4096 :: Double
  let  x = UV.fromList $ take (truncate (fs*10)) $ downsample fs' fs $ map realToFrac (eval fdata)


  print "{- calculate spectrum -}"
  let nfft = truncate fs :: Int
  out <- gwpsd x nfft fs
  let (fvec, psdvec) = UV.unzip out
  HR.plot HR.LogXY HR.Line 1 ("frequency","Spectrum") 0.05 "psd" "test_psd.png" ((0,0),(0,0))
    $ Prelude.zip (UV.toList fvec) (UV.toList psdvec)


