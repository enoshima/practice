import Control.Monad.State
import Control.Monad

main :: IO ()
main = runStateT code [1..] >> return ()

code :: StateT [Integer] IO ()
code = do
   forM_ [5, 6, 7::Int] $ \i -> do
    io $ print i
    x <- pop
    io $ print x
    return ()

pop :: StateT [Integer] IO Integer
pop = do
  (x:xs) <- get
  put xs
  return x

io :: IO a -> StateT [Integer] IO a
io = liftIO
