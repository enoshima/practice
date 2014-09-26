
module FilterUnboxed where


import Data.Array.Unboxed

iir_df2 :: (Num a) => (Array Int a, Array Int a) -- ^ (b,a)
     -> [a] -- ^ x[n]
     -> [a] -- ^ y[n]

iir_df2 (b,a) x = y
    where y = iir'df2 (b,a) w x
          w = listArray (0,mn) $ repeat 0
          m = snd $ bounds b
          n = snd $ bounds a
          mn = max m n

{- specialize iir'df2 :: Array Int Float -> Array Int Float -> [Float] -> [Float] -}
{- specialize iir'df2 :: Array Int Double -> Array Int Double -> [Double] -> [Double] -}

iir'df2 :: (Num a) => (Array Int a,Array Int a) -> Array Int a -> [a] -> [a]
iir'df2 _     _ []     = []
iir'df2 (b,a) w (x:xs) = y : iir'df2 (b,a) w' xs
    where y  = sum [ b!i * w'!i | i <- [0..m] ]
          w0 = x - sum [ a!i * w'!i | i <- [1..m] ]
          w' = listArray (0,mn) $ w0 : elems w
          m  = snd $ bounds b
          mn = snd $ bounds w
