module WindowFunction

( module WindowType
, windowed
, hanning
, blackman
, hamming
, tukeywin
, kaiser
)
where

import Numeric.GSL.Special hiding (cos)
import WindowType
import qualified Data.Vector.Unboxed as UV

windowed :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double
windowed = UV.zipWith (*)

hanning :: Int -> UV.Vector Double
hanning = makeWindow hanning'

blackman :: Int -> UV.Vector Double
blackman = makeWindow blackman'

hamming :: Int -> UV.Vector Double
hamming = makeWindow hamming'

tukeywin :: Double -> Int -> UV.Vector Double
tukeywin r = makeWindow (tukeywin' r)

kaiser :: Double -> Int -> UV.Vector Double
kaiser a = makeWindow (kaiser' a)


-------  internal functions below ----------
makeWindow :: (Double -> Double -> Double) -> Int -> UV.Vector Double
makeWindow win m =
    let md = fromIntegral m
    in UV.fromList $ map (win md . fromIntegral) [(0::Int)..(m-1::Int)]

hanning' :: Double -> Double -> Double
hanning' m n = 0.5 - 0.5 * cos(2 * pi * n / m)

blackman' :: Double -> Double -> Double
blackman' m n = 0.42 - 0.5 * cos(2 * pi * n / m) + 0.08*cos(4 * pi * n / m)

hamming' :: Double -> Double -> Double
hamming' m n = 0.54 - 0.46 * cos(2 * pi * n / m)

tukeywin' :: Double -> Double -> Double -> Double
tukeywin' r m n
  | r<=0&&r>=1 = hamming' m n
  | otherwise = tukeywinCore r m n

tukeywinCore :: Double -> Double -> Double -> Double
tukeywinCore r m n
  | x>=0&&x<r/2 = 0.5 * (1 + cos(2 * pi / r * (x - r / 2)))
  | x>=r/2&&x<1-r/2 = 1
  | x>=1-r/2&&x<=1 = 0.5 * (1 + cos(2 * pi / r * (x - 1 + r / 2)))
  | otherwise = error "not working with current implementation."
  where x = n/m

kaiser' :: Double -> Double -> Double -> Double
kaiser' a m n = bessel_J0(pi * a * sqrt(1-(2*x-1)**2))/bessel_J0(pi*a)
  where x = n/m



