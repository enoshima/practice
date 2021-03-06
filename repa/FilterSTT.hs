{-# LANGUAGE BangPatterns #-}

module FilterSTT
(
  iir
, fir
) where

import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import Control.Monad.State


{- external functions -}
iir :: ([Double], [Double])  -- ^ (numerator, denominator)
    -> UV.Vector Double      -- ^ target time-series data
    -> IO (UV.Vector Double) -- ^ filtered time-series
iir (b, a) v = evalStateT go w
  where go = iirState (b, a) v
        w = UV.replicate mn 0 :: UV.Vector Double
        mn = max na nb
        na = length a
        nb = length b


{- internal functions -}
iirState :: ([Double], [Double])                            -- ^ (numerator, denominator)
         -> UV.Vector Double                                -- ^ target time-series data
         -> StateT (UV.Vector Double) IO (UV.Vector Double) -- ^ filtered time-series data
iirState (b, a) v = do
  let bv = UV.fromList b :: UV.Vector Double
      av = UV.fromList a :: UV.Vector Double
      na = length a
      nb = length b
      mn = max na nb
  UV.forM v $ \x -> do
    w <- get
    pop' (x - UV.sum (UV.zipWith (*) (UV.drop 1 av) (UV.take (mn-1) w)))
    w <- get
    let y = UV.sum $ UV.zipWith (*) bv w
    return y


pop :: Double -> UV.Vector Int -> StateT (UV.Vector Double) IO ()
pop w0 perm = do
  w <- get
  let w' = UV.modify (\r -> UMV.unsafeWrite r 0 w0) (UV.backpermute w perm)
  put w'
  return ()

pop' :: Double -> StateT (UV.Vector Double) IO ()
pop' w0 = do
  !w <- get
--  let w' = UV.modify (\r -> UMV.unsafeWrite r 0 w0) (UV.backpermute w perm)
  let !w' = GV.cons w0 (UV.init w)
  put w'
  return ()



fir :: [Double] -> UV.Vector Double -> IO (UV.Vector Double)
fir b v = evalStateT go w
  where !go = firState b v
        !w = UV.replicate nb 0 :: UV.Vector Double
        !nb = length b

firState :: [Double] -> UV.Vector Double -> StateT (UV.Vector Double) IO (UV.Vector Double)
firState b v = do
  let !bv = UV.fromList b :: UV.Vector Double
  UV.forM v $ \x -> do
    !w <- get
    pop' x
    !w <- get
    let !y = UV.sum (UV.zipWith (*) bv w)
    return y



