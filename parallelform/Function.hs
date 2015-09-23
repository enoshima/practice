


module Function
( polyval
, toeplitz
, tf2cparallelForm
, tf2rparallelForm
) where


import Numeric.LinearAlgebra
import Numeric.GSL.Polynomials(polySolve)
import Data.Complex

-- | calculating polynomials a0 + a1x^1+ .. + anx^n at x = x0
-- | polyval [a0, a1, a2..an] x0
polyval :: Num a => [a] -> a -> a
polyval [] _ = 0
polyval (x:xs) x0 = x + x0 * polyval xs x0


-- | calculating Toeplitz matrix
-- | output : list of column [list]
toeplitz :: Num a => [a] -> [a] -> [[a]]
toeplitz c r = do
  let nrow = length c
      ncol = length r
   in map (\i -> go c r i ) [0..ncol-1]
   where
     go x y i
       | i==0 = x
       | otherwise = take nx $ (reverse . take i $ drop 1 y) ++ reverse (drop i (reverse c))
       where nx = length x


tf2rparallelForm :: ([Double], [Double]) -> ([Double], [([Double], [Double])])
tf2rparallelForm (num, denom) = do
  let (c, gain, alpha) = tf2cparallelForm (num, denom)
   in (c, func gain alpha)
   where
     func :: [Complex Double] -> [Complex Double] -> [([Double], [Double])]
     func _ [] = []
     func gain alpha = do
       let hal = head alpha
           hA  = head gain
        in case (imagPart hal ==0) of
             False -> ([2*realPart hA, -2*realPart (hA * conjugate hal)]
               , [1, -2*realPart hal, (realPart (abs hal))**2]) : func (drop 2 gain) (drop 2 alpha)
             True  -> ([realPart (hA), 0], [1, -1*realPart hal, 0]) : func (tail gain) (tail alpha)



tf2cparallelForm :: ([Double], [Double]) -> ([Double], [Complex Double], [Complex Double])
tf2cparallelForm (num', denom') = do
  let num = map (/head denom') num'
      denom = map (/head denom') denom'
      p = length denom - 1
      q = length num - 1
      (c, d) = case (q >= p) of
        True -> do
          let temp = toeplitz (denom ++ replicate (q-p) (0::Double))
                (head denom : replicate (q-p) (0::Double))
              tempM = fromColumns $ map fromList temp
              numM = ((q+1) >< 1) num
              denomM = ((p+1) >< 1) denom
              zeros = replicate (q-p+1) (fromList $ replicate p (0::Double))
              eye = ident p :: Matrix Double
              temp' = fromBlocks [[tempM, fromRows (toRows eye ++ zeros)]]
              temp''= temp' <\> numM
              c = toList . head . toColumns $ subMatrix (0, 0) (q-p+1, 1) temp''
              d' = toList . head . toColumns $ subMatrix (q-p+1, 0) (p, 1) temp''
           in (c, d2clist d')
        False -> do
          let c = []
              d' = num ++ replicate (p-q-1) (0::Double)
           in (c, d2clist d')

  let alpha = polySolve $ reverse denom
      gpf = map (\i->go i) [0..length alpha -1]
        where
          go i = do
            let scale = product [alpha!!i - x | x <- alpha, x /= alpha!!i]
             in (polyval (reverse d) (alpha!!i)) / scale
   in (c, gpf, alpha)


d2clist :: [Double] -> [Complex Double]
d2clist xs = map (\x->x:+0) xs
