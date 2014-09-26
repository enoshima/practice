

{- looking into high performance FIR filter -}

import qualified Data.Vector.Unboxed asUV
import System.Random


main = do

  let input = UV.take 1000 $ UV.fromList $ randomRs (-1, 1) $ mkStdGen 1 :: UV.Vector Double
      filterLen = 10 :: Int
      b = UV.replicate filterLen 1
      a = UV.replicate filterLen (-0.5)
      {- initial registers -}
      regInit = UV.replicate filterLen 0.0
      {- initializing the delay registers -}
      reg = regInit
      {- set input to reg0 -}
  map go [0..length input -1]
    where
      go = \x -> sum $ UV.zipWith (*) b tmpReg
       where
        tmpReg = UV.take (fulterLen+1) UV.cons reg0
        reg0 = x - sum $ zipWith (*) (UV.tail a) (UV.tail


