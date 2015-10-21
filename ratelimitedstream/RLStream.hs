{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-unused-imports #-}



module RLStream
 (
   Stream, streamFromList, streamMap, streamFold, streamFilter
 ) where


import Control.Monad.Par.Scheds.Trace as P
import Control.DeepSeq
import Data


streamFromList :: NFData a => Int -> [a] -> Par (Stream a)
streamFromList n xs = do
  var <- new
  fork $ loop n xs var
  return var
  where
    loop k [] var = put var Nil
    loop k xs var | k <= 0 = do
      tail <- new
      put var (Fork (loop n xs tail) tail)
    loop k (x:xs) var = do
      tail <- new
      put var (Cons x tail)
      loop (k-1) xs tail


streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn inp = do
  out <- new
  fork $ loop out inp
  return out
  where
    loop out inp = do
      x <- get inp
      case x of
        Nil         -> put out Nil
        Cons a inp' -> do out' <- new
                          put out (Cons (fn a) out')
                          loop out' inp'
        Fork p inp' -> fork p >> loop out inp'


streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc inp = do
  x <- get inp
  case x of
    Nil         -> return acc
    Cons b inp' -> streamFold fn (fn acc b) inp'
    Fork p inp' -> fork p >> streamFold fn acc inp'


streamFilter :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
streamFilter p inp = do
  out <- new
  fork $ loop out inp
  return out
  where
    loop out inp = do
      x <- get inp
      case x of
        Nil -> put out Nil
        Cons a inp'
          | p a       -> do out' <- new
                            put_ out (Cons a inp')
                            loop out' inp'
          | otherwise -> loop out inp'
        Fork p inp'   -> fork p >> loop out inp



