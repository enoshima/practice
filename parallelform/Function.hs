


module Function
( polyval
, toeplitz
) where


import Numeric.LinearAlgebra
import Numeric.GSL.Polynomials(polySolve)
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.Chebyshev
import HasKAL.SignalProcessingUtils.FilterType
import FilterKind

-- | calculating polynomials a0 + a1x^1+ .. + anx^n at x = x0
-- | polyval [a0, a1, a2..an] x0
polyval :: Num a => [a] -> a -> a
polyval [] _ = 0
polyval (x:xs) x0 = x + x0 * polyval xs x0


-- | calculating Toeplitz matrix
-- |
toeplitz :: Num a => [a] -> [a] -> [[a]]
toeplitz c r = do
  let nrow = length c
      ncol = length r
   in map (\i -> go c r i ) [0..nrow-1]
   where
     go x y i
       | i==0 = x
       | otherwise = (reverse . take i $ drop 1 y) ++ drop i c


tf2parallelForm (num, denom) = do
  let p = length denom - 1
      q = length num - 1
  (c, d) <- case (q >= p) of
    True -> do
      let temp = toeplitz (denom ++ replicate (q-p) 0::Double)
            (head denom ++ repeat 0 (q-p))
          numM = (q+1><1) num
          denomM = (p+1><1) denom
          zeros = replicate (q-p+1) (fromList $ replicate p 0::Double)
          eye = ident p :: Matrix Double
          temp' = fromBlocks [[denomM, fromRows (toRows eye ++ zeros)]]
          temp''= temp' <\> numM
          c = toList . head . toRows $ subMatrix (0, 0) (1, q-p+1) temp''
          d = toList . head . toRows $ subMatrix (0, q-p+1) (1, p) temp''
      return (c, d)
    False -> do
      let c = []
          d = num ++ replicate (p-q+1) 0
      return (c, d)

  let alpha = polySolve $ reverse denom
      gpf = map (\i -> product [alpha!!i - x | x <- alpha, x /= alpha!!i]) [0..length alpha -1]



