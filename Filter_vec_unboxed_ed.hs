

{-
- Haskell Filter tools
-}


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Filter_vec_unboxed_ed
( fir
, iir_df2
) where

import qualified Data.Vector.Unboxed as UV

-- | iir numerator denominator input
iir_df2 :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double
iir_df2 b a x = y
  where
    y = iir'df2 b a w x
    w = UV.replicate mn 0
    m = UV.length b
    n = UV.length a
    mn = max m n

iir'df2 :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double
iir'df2 b a w v = do
  case v of
    Nils
      -> UV.fromList []
    Cons x xs
      -> UV.cons y (iir'df2 b a w' xs)
      where y = UV.sum $ UV.zipWith (*) b w'
            w0 = x - (UV.sum $ UV.zipWith (*) (UV.tail a) (UV.init w))
            w' = UV.cons w0 (UV.init w)





-- | fir coefficients input
fir :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double
fir h v = do
  case v of
    Nils -> UV.fromList []
    Cons x xs
      | isFIRType1 h -> fir'1 h w xs
      | isFIRType2 h -> fir'2 h w xs
      | isFIRType3 h -> fir'3 h w xs
      | isFIRType4 h -> fir'4 h w xs
      | otherwise    -> fir'0 h w xs
      where
        w = UV.cons x (UV.replicate m 0)
        m = UV.length h - 1




fir'0 :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double
fir'0 h w v = do
  case v of
    Cons x xs
      -> UV.cons y (fir'0 h w' xs)
      where y = sum [h UV.!i * w UV.!i | i<-[0..m]]
            w' = UV.cons x w
            m = UV.length h - 1
    Nils
      -> UV.fromList [y]
      where y = sum [h UV.!i * w UV.!i | i<-[0..m]]
            m = UV.length h - 1

fir'1 :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double
fir'1 h w v = do
  case v of
    Cons x xs
      -> UV.cons y (fir'1 h w' xs)
      where y = h UV.!m2 * w UV.!m2 + sum [h UV.!i *(w UV.!i + w UV.!(m-i)) | i<-[0..m2-1]]
            w' = UV.cons x w
            m = UV.length h - 1
            m2 = div m 2
    Nils
      -> UV.fromList [y]
      where y = h UV.!m2 * w UV.!m2 + sum [h UV.!i *(w UV.!i + w UV.!(m-i)) | i<-[0..m2-1]]
            m = UV.length h - 1
            m2 = div m 2

fir'2 :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double
fir'2 h w v = do
  case v of
    Cons x xs
      -> UV.cons y (fir'2 h w' xs)
      where y = sum [h UV.!i * (w UV.!i + w UV.!(m-i)) | i<-[0..m2]]
            w' = UV.cons x w
            m = UV.length h - 1
            m2 = div m 2
    Nils -> UV.fromList [y]
      where y = sum [h UV.!i * (w UV.!i + w UV.!(m-i)) | i<-[0..m2]]
            m = UV.length h - 1
            m2 = div m 2

fir'3 :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double
fir'3 h w v = do
  case v of
    Cons x xs
      -> UV.cons y (fir'3 h w' xs)
      where y = h UV.!m2 * w UV.!m2 + sum [h UV.!i *(w UV.!i - w UV.!(m-i)) | i<-[0..m2-1]]
            w' = UV.cons x w
            m = UV.length h - 1
            m2 = div m 2
    Nils -> UV.fromList [y]
      where y = h UV.!m2 * w UV.!m2 + sum [h UV.!i *(w UV.!i - w UV.!(m-i)) | i<-[0..m2-1]]
            m = UV.length h - 1
            m2 = div m 2

fir'4 :: UV.Vector Double -> UV.Vector Double -> UV.Vector Double -> UV.Vector Double
fir'4 h w v = do
  case v of
    Cons x xs
      -> UV.cons y (fir'4 h w' xs)
      where y = sum [h UV.!i * (w UV.!i - w UV.!(m-i)) | i<-[0..m2]]
            w' = UV.cons x w
            m = UV.length h - 1
            m2 = div m 2
    Nils
      -> UV.fromList [y]
      where y = sum [h UV.!i * (w UV.!i - w UV.!(m-i)) | i<-[0..m2]]
            m = UV.length h - 1
            m2 = div m 2




uncons :: UV.Unbox a => UV.Vector a -> Maybe (a, UV.Vector a)
uncons v = do
  case UV.null v of
    False -> Just (UV.head v, UV.tail v)
    True  -> Nothing

pattern Cons x xs <- (uncons -> Just (x, xs))
pattern Nils <- (uncons -> Nothing)



isFIRType1 :: UV.Vector Double -> Bool
isFIRType1 h = even m && (h == (UV.reverse h))
  where m = UV.length h -1

isFIRType2 :: UV.Vector Double -> Bool
isFIRType2 h = odd m && (h == (UV.reverse h))
  where m = UV.length h -1

isFIRType3 :: UV.Vector Double -> Bool
isFIRType3 h = even m && (ha == UV.reverse hb)
  where m = UV.length h -1
        ha = UV.take n h
        hb = UV.map (*(-1)) (UV.drop (n+1) h)
        n = div m 2

isFIRType4 :: UV.Vector Double -> Bool
isFIRType4 h = odd m && (h == UV.reverse hb)
  where m = UV.length h -1
        hb = UV.map (*(-1)) h






