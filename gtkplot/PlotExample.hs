{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}

module Main where

import qualified Control.Concurrent as CC
import GHC.Generics ( Generic )
--import qualified System.Remote.Monitoring as EKG

import PlotHo ( Lookup, XAxisType(..), runPlotter, addHistoryChannel )

data Foo a = MkFoo { x :: Double
                   , y :: Double
                   , z :: Double
                   , w :: a
                   } deriving Generic
data Bar = MkBar { lol :: Double
--                   , xyzList :: S.Seq Xyz
                 , foos :: Foo (Foo (Foo Double))
                 , foo :: Foo Double
                 } deriving Generic
instance Lookup a => Lookup (Foo a)
instance Lookup Bar

bar0 :: Bar
bar0 =
  MkBar
  7
--  (S.fromList [MkFoo 1 2 3])
  (MkFoo 1 2 3 (MkFoo 4 5 6 (MkFoo 7 8 9 10)))
  (MkFoo 1 2 3 4)

foo0 :: Foo Double
foo0 = MkFoo 1 2 3 0.1

incrementBar :: Bar -> Bar
incrementBar (MkBar a _ _) =
  MkBar
  (a+0.2)
  (foo' (foo' (foo' (sin (3*a)))))
  (foo' (sin (2*a)))
  where
    foo' t = MkFoo (sin a) (cos a) (sin a * cos a) t

incrementFoo :: Foo a -> Foo a
incrementFoo (MkFoo a _ _ b) = MkFoo (a+0.3) (2 * sin a) (3 * cos a) b

-- a random function to write a bunch of data to a chan
channelWriter :: Int -> Int -> (a -> a) -> a -> (a -> Bool -> IO ()) -> IO ()
channelWriter count delay increment x' chan = do
  --putStrLn $ "writing: " ++ show count
  CC.threadDelay delay
  chan (increment x') False
  channelWriter (count + 1) delay increment (increment x') chan



sameSignalTree :: forall a . a -> a -> Bool
sameSignalTree a a = True

toSignalTree gps a = case accessor of
  (Field _) -> error "toSignalTree: got a Field right away"
  d -> Tree.subForest $ head $ makeSignalTree [] d
  where
    makeSignalTree :: [String] -> AccessorTree a -> HistorySignalTree a
    makeSignalTree myFieldName (Data (ptn,_) children) =
      [Tree.Node
       (reverse myFieldName, Left ptn)
       (concatMap (\(getterName, child) -> makeSignalTree (getterName:myFieldName) child) children)
      ]
    makeSignalTree myFieldName (Field field) =
      [Tree.Node (reverse myFieldName, Right (toGetter (toDoubleGetter field))) []]

toDoubleGetter :: Field a -> (a -> Double)
toDoubleGetter (FieldDouble f) = (^. f)
toDoubleGetter (FieldFloat f) = realToFrac . (^. f)
toDoubleGetter (FieldBool f) = fromIntegral . fromEnum . (^. f)
toDoubleGetter (FieldInt f) = fromIntegral . (^. f)
toDoubleGetter (FieldString _) = const 0
toDoubleGetter FieldSorry = const 0

toGetter :: (S.Seq (a, Double) -> Double) -> S.Seq (a, Double) -> [[(Double,Double)]]
toGetter get s = [map \(val, time) -> (time, get val)) (F.toList s)]





main :: IO ()
main = do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000

  runPlotter $ do
    addChannel "MyChannel" sameSignalTree toSignalTree $ mychannelWriter 
    addHistoryChannel "Foo (XAxisTime)"   XAxisTime   $ channelWriter 0 50000 incrementFoo foo0
    addHistoryChannel "Bar (XAxisCount)"  XAxisCount  $ channelWriter 0 60000 incrementBar bar0
    addHistoryChannel "Foo (XAxisTime0)"  XAxisTime0  $ channelWriter 0 50000 incrementFoo foo0
    addHistoryChannel "Bar (XAxisCount0)" XAxisCount0 $ channelWriter 0 60000 incrementBar bar0
