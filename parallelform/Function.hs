


module Function
( polyval
, toeplitz
, tf2cparallel
, tf2rparallel
, tf2cascade
) where


import Numeric.LinearAlgebra
import Numeric.GSL.Polynomials(polySolve)
import Data.List
import Data.Maybe (fromJust)
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


tf2rparallel :: ([Double], [Double]) -> ([Double], [([Double], [Double])])
tf2rparallel (num, denom) = do
  let (c, gain, alpha) = tf2cparallel (num, denom)
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


tf2cparallel :: ([Double], [Double]) -> ([Double], [Complex Double], [Complex Double])
tf2cparallel (num', denom') = do
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


tf2cascade :: ([Double], [Double]) -> [([Double], [Double])]
tf2cascade (num, denom) = do
  let zeroz = polySolve $ reverse num
      polez = polySolve $ reverse denom
      polec' = [x | x <- polez, imagPart x>0]
      poler' = [x | x <- polez, imagPart x==0]
      zeroc = [x | x <- zeroz, imagPart x>0]
      zeror = [x | x <- zeroz, imagPart x==0]
      polec = reverse . cmplxSort $ polec'
      poler = reverse . cmplxSort $ poler'
      secDenom = fsecDenom polec
      secNum = fsecNum polec zeroc zeror
   in out poler zeror secNum secDenom
   where
     out poler zeror secNum secDenom
      | length poler == 0 = zip secNum secDenom
      | length poler == 1 = zip secNum secDenom
        ++ [([1, -realPart (last zeror), 0], [1, -realPart (last poler), 0])]
      | length poler == 2 = zip secNum secDenom
        ++ [([1, -realPart (last poler)-realPart (last.init $ poler)
           , -(realPart (last poler))*realPart (last.init  $ poler)]
           , [1, -realPart (last zeror)-realPart (last.init $ zeror)
           , -(realPart (last zeror))*realPart (last.init $ zeror)])]
      | otherwise = error "something wrong,  perhaps you have >2 real zero"


d2clist :: [Double] -> [Complex Double]
d2clist xs = map (\x->x:+0) xs


--tapleSort :: (Num a, Num b) =>  [(a, b)] -> [(a, b)]
cmplxSort :: [Complex Double] -> [Complex Double]
cmplxSort [] = []
cmplxSort (r:+c:xs) = cmplxSort lt ++ [r:+c] ++ cmplxSort gteq
                        where
                          lt = [r':+c' | r':+c' <- xs, abs r' < abs r]
                          gteq=[r':+c' | r':+c' <- xs, abs r' >= abs r]


fsecDenom :: [Complex Double] -> [[Double]]
fsecDenom [] = []
fsecDenom polec = [1, -2*realPart (head polec), (realPart (abs (head polec)))**2.0] : fsecDenom (tail polec)


fsecNum :: [Complex Double] -> [Complex Double] -> [Complex Double] -> [[Double]]
fsecNum [] _ _ = []
fsecNum polec zeroc zeror =
  case length zeroc >0 of
    True -> let a = map (\i-> realPart (abs (head polec - i))) zeroc
                mina = minimum a
                ind = fromJust $ elemIndex mina a
             in [1, -2*realPart (zeroc!!ind), (realPart (abs (zeroc!!ind)))**2.0]
                     : fsecNum (tail polec) (delete (zeroc!!ind) zeroc) zeror
    False-> let a1 = map (\i-> realPart (abs (head polec - i))) zeror
                mina1 = minimum a1
                ind1 = fromJust $ elemIndex mina1 a1
                tmpsecn1 = [1, -zeror!!ind1]
                zeror1 = delete (zeror!!ind1) zeror
                a2 = map (\i-> realPart (abs (head polec - i))) zeror1
                mina2 = minimum a2
                ind2 = fromJust $ elemIndex mina2 a2
                tmpsecn2 = map realPart $ convolve tmpsecn1 [1, -zeror1!!ind2]  -- ToDo Check it
                zeror2 = delete (zeror1!!ind2) zeror1
             in tmpsecn2 : fsecNum (tail polec) zeroc zeror2


convolve :: (Num a) => [a] -> [a] -> [a]
convolve hs xs =
  let pad = replicate ((length hs) - 1) 0
      ts  = pad ++ xs
   in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)












