

module Resampling
( downsample
, upsample
, resample
) where

import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.ButterWorth
import Filter

downsample :: Double -> Double -> [Double] -> [Double]
downsample fs newfs x = y
  where y = snd.unzip $ filter (\(n, _) -> n `mod` p==1) $ zip [1..] x'
        p = truncate (fs/newfs)
        x' = iir_df2 lpf x
        lpf = butter 2 fs (newfs/2) Low


upsample :: Double -> Double -> [Double] -> [Double]
upsample fs newfs x = undefined


resample :: Double -> Double -> [Double] -> [Double]
resample fs newfs x = undefined



