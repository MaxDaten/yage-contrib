{-# LANGUAGE LiberalTypeSynonyms #-}
module Yage.Lens where

import qualified   Prelude
import Data.Maybe

import Control.Lens


-- | TODO a maybe modify operator
(%?~) :: Profunctor p => Simple ASetter s a -> p a a -> s -> s
l %?~ mf = Prelude.undefined -- over l (maybe l )

