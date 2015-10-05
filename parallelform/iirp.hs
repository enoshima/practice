


module IIRp
(iirp)
where


import Control.Concurrent.Async (async, wait)
--import Control.DeepSeq (deepseq)
import Data.List (foldl1')
import qualified Data.Vector.Storable as VS
import Numeric.LinearAlgebra()
import HasKAL.SignalProcessingUtils.Filter
import System.IO.Unsafe (unsafePerformIO)


iirp :: ([Double], [([Double], [Double])]) -> VS.Vector Double -> VS.Vector Double
iirp (firpart, iirpart) v = case null firpart of
  True  -> applyIIR iirpart v
  False -> applyFIRIIR firpart iirpart v


applyIIR :: [([Double], [Double])] -> VS.Vector Double -> VS.Vector Double
applyIIR coeffs v = unsafePerformIO $ do
  jobs <- mapM (\c -> async (return $ sos1filter c v)) coeffs
  outs <- mapM wait jobs
  return $ foldl1' (+) outs


applyFIRIIR :: [Double] -> [([Double], [Double])] -> VS.Vector Double -> VS.Vector Double
applyFIRIIR firpart iirpart v = unsafePerformIO $ do
  firjob <- async (return $ fir firpart v)
  iirjobs<- mapM (\c -> async (return $ sos1filter c v)) iirpart
  outs <- mapM wait (firjob : iirjobs)
  return $ foldl1' (+) outs


