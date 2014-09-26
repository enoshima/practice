{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import HasKAL.DetectorUtils.Detector
import HasKAL.TimeUtils.Signature

data Foo a = Foo { _bar :: Int
                 , _baz :: Int
                 , _quux :: a} deriving (Eq, Show)
makeLenses ''Foo

data GWDATA = GWDATA { _detector :: Detector
                     , _dataType :: String
                     , _samplingFrequency :: Double
                     , _startGPSTime :: GPSTIME
                     , _stopGPSTime  :: GPSTIME
                     , _gwdata :: [Double]
                     } deriving (Show, Eq, Read)
$(makeLenses ''GWDATA)

