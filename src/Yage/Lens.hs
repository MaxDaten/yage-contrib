{-# LANGUAGE TypeFamilies #-}
module Yage.Lens (
      module Yage.Lens
    , module Lens
    ) where

import Prelude (id, const)
import Data.Maybe
import Data.Trie as Trie
import Data.ByteString

import Control.Applicative (pure)

import Control.Lens as Lens

infixr 4 %?~


-- | TODO a maybe modify operator
(%?~) :: ASetter' s a -> Maybe a -> s -> s
l %?~ mf = over l (\a -> maybe a id mf)
{-# INLINE (%?~) #-}

type instance Index (Trie a) = ByteString

type instance IxValue (Trie a) = a
instance Ixed (Trie a) where
  ix k f m = case Trie.lookup k m of
     Just v -> f v <&> \v' -> Trie.insert k v' m
     Nothing -> pure m
  {-# INLINE ix #-}


instance At (Trie a) where
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (Trie.delete k m)) mv
    Just v' -> Trie.insert k v' m
    where mv = Trie.lookup k m
  {-# INLINE at #-}