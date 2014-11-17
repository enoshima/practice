
{-# LANGUAGE ViewPatterns #-}

module IIRr
(
  iir
) where

import Data.Array.Repa
import qualified Data.Array.Repa as Repa
import qualified Data.Vector.Unboxed as UV
--import Control.Applicative


iir :: ([Double], [Double])  -- ^ (b: numerator, a: denominator)
    -> UV.Vector Double      -- ^ target time-series
    -> UV.Vector Double -- ^ filtered time-series
iir (b, a) v = Repa.toUnboxed $ iir' (b', a') w v'
  where v' = Repa.fromUnboxed (Repa.ix1 (UV.length v)) v
        w = Repa.fromListUnboxed (Repa.ix1 mn) $ replicate mn 0
        b'= Repa.fromUnboxed (Repa.ix1 nb) (UV.fromList b)
        a'= Repa.fromUnboxed (Repa.ix1 na) (UV.fromList a)
        mn = max nb na
        na = length a
        nb = length b

iir' :: (Repa.Array Repa.U Repa.DIM1 Double,Repa.Array Repa.U Repa.DIM1 Double)
     -> Repa.Array Repa.U Repa.DIM1 Double
     -> Repa.Array Repa.U Repa.DIM1 Double
     -> Repa.Array Repa.U Repa.DIM1 Double
iir' _ _ (isNull -> True) = fromListUnboxed (Z:.0) []
iir' (b, a) w v = do
    let na = Repa.size (Repa.extent a)
        nw = Repa.size (Repa.extent w)
        nv = Repa.size (Repa.extent v)
        ina = computeS (extract (Z:.1) (Z:.(na-1)) a) :: Repa.Array Repa.U Repa.DIM1 Double
        inw = computeS (extract (Z:.0) (Z:.(nw-1)) w) :: Repa.Array Repa.U Repa.DIM1 Double
        w0' = computeS (ina *^ inw) :: Repa.Array Repa.U Repa.DIM1 Double
        sumw0' = sumAllS w0'
    let w0 = fromListUnboxed (Z:.1) [v!(Z:.0)-sumw0'] :: Repa.Array Repa.U Repa.DIM1 Double
        w' = computeS (w0 Repa.++ extract (Z:.0) (Z:.(nw-1)) w) :: Repa.Array Repa.U Repa.DIM1 Double
        v' = computeS (extract (Z:.1) (Z:.(nv-1)) v) :: Repa.Array Repa.U Repa.DIM1 Double
        y' = computeS (b *^ w') :: Repa.Array Repa.U Repa.DIM1 Double
        sumy = sumAllS y'
        y = fromListUnboxed (Z:.1) [sumy] :: Repa.Array Repa.U Repa.DIM1 Double
    computeS ( y Repa.++ iir' (b, a) w' v') :: Repa.Array Repa.U Repa.DIM1 Double

isNull :: Repa.Array Repa.U Repa.DIM1 Double -> Bool
isNull
  | v == fromListUnboxed (Z:.0) [] = True
  | otherwise = False
