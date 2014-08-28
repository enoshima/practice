{-# LANGUAGE TemplateHaskell #-}

module TestLens
where

import Control.Lens
import HROOT

data AAA = AAA 
  { _data :: Double
  } deriving (Show,Eq,Read)
$(makeLenses ''AAA)


