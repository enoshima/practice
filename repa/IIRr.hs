module IIRr
(
iir
) where

import Data.Array.Repa
import qualified Data.Array.Repa as Repa
import qualified Data.Vector.Unboxed as UV
import Control.Applicative


iir :: ([Double], [Double])  -- ^ (b: numerator, a: denominator)
    -> UV.Vector Double      -- ^ target time-series
    -> IO (UV.Vector Double) -- ^ filtered time-series
iir (b, a) v = Repa.toUnboxed <$> iir' (b', a') w v'
  where v' = Repa.fromUnboxed (Repa.ix1 (UV.length v)) v
        w = Repa.fromListUnboxed (Z:.(1::Int)) $ replicate mn 0
        b'= Repa.fromUnboxed (Repa.ix1 (length b)) (UV.fromList b)
        a'= Repa.fromUnboxed (Repa.ix1 (length a)) (UV.fromList a)
        mn = max (length b) (length a)

iir' :: (Repa.Array Repa.U Repa.DIM1 Double,Repa.Array Repa.U Repa.DIM1 Double)
     -> Repa.Array Repa.U Repa.DIM1 Double
     -> Repa.Array Repa.U Repa.DIM1 Double
     -> IO (Repa.Array Repa.U Repa.DIM1 Double)
iir' (b, a) w v
  | v == fromListUnboxed (Z:.(0::Int)) [] = return $ fromListUnboxed (Z:.(1::Int)) []
  | otherwise = do
    y' <- computeP (b *^ w) :: IO (Repa.Array Repa.U Repa.DIM1 Double)
    sumy <- sumAllP y'
    let y = fromListUnboxed (Z:.(1::Int)) [sumy] :: Repa.Array Repa.U Repa.DIM1 Double
        lena = Repa.size (Repa.extent a)
        ina = computeS (extract (Z:.(1::Int)) (Z:.((lena-1)::Int)) a) :: Repa.Array Repa.U Repa.DIM1 Double
        inw = computeS (extract (Z:.(1::Int)) (Z:.((lena-1)::Int)) w) :: Repa.Array Repa.U Repa.DIM1 Double
    w0' <- computeP (ina *^ inw) :: IO (Repa.Array Repa.U Repa.DIM1 Double)
    sumw0' <- sumAllP w0'
    let w0 = fromListUnboxed (Z:.(1::Int)) [v!(Z:.(0::Int))-sumw0'] :: Repa.Array Repa.U Repa.DIM1 Double
        lenw = Repa.size (Repa.extent w)
        lenv = Repa.size (Repa.extent v)
    w' <- computeP (w0 Repa.++ extract (Z:.(0::Int)) (Z:.((lenw-1)::Int)) w) :: IO (Repa.Array Repa.U Repa.DIM1 Double)
    v' <- computeP (extract (Z:.(0::Int)) (Z:.((lenv-1)::Int)) v) :: IO (Repa.Array Repa.U Repa.DIM1 Double)
    next <- iir' (b, a) w' v'
    computeP ( y Repa.++ next) :: IO (Repa.Array Repa.U Repa.DIM1 Double)
