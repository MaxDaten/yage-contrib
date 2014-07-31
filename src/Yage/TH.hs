{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.TH where

import Yage.Prelude

import Language.Haskell.TH.Syntax

instance Lift FilePath where
    lift fp = [| fpToString fp |]