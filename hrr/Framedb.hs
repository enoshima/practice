{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Framedb where

import Prelude hiding (id)
import DataSource (defineTable)

$(defineTable
    []
    "KAGRA" "framedb" [])
