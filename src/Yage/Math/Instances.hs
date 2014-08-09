{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.Math.Instances where

import Linear
import Data.Aeson.TH

$(deriveJSON defaultOptions ''V0)
$(deriveJSON defaultOptions ''V1)
$(deriveJSON defaultOptions ''V2)
$(deriveJSON defaultOptions ''V3)
$(deriveJSON defaultOptions ''V4)
