


module Function
( polyval
, toeplitz
) where

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


