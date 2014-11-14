


import qualified Foreign.ForeignPtr.Safe as FPS
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as RFP
import Data.Array.Repa
import Foreign.Ptr


ptr2repa :: Int -> Ptr Double -> IO (Repa.Array RFP.F Repa.DIM1 Double)
ptr2repa len p = do
  fp <- FPS.newForeignPtr_ p
  return $ RFP.fromForeignPtr (Z :. len) fp
