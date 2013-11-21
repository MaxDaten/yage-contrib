{-# LANGUAGE LiberalTypeSynonyms #-}
module Yage.Lens (
      module Yage.Lens
    , module Lens
    ) where

import qualified   Prelude
import Data.Maybe

import Control.Lens as Lens

infixr 4 %?~


-- | TODO a maybe modify operator
(%?~) :: Simple ASetter s a -> Maybe a -> s -> s
l %?~ mf = over l (\a -> maybe a Prelude.id mf)
{-# INLINE (%?~) #-}
