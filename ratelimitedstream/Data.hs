
module Data
where


import Control.Monad.Par.Scheds.Trace
import Control.DeepSeq


data IList a
  = Nil
  | Cons a        (IVar (IList a))
  | Fork (Par ()) (IVar (IList a))

type Stream a = IVar (IList a)

instance NFData a => NFData (IList a) where
--  rnf Nil = r0
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork _ a) = rnf a

