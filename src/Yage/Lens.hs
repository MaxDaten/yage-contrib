{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Yage.Lens (
      module Yage.Lens
    , module Lens
    ) where

import Prelude (id, const, flip, uncurry)
import Data.Maybe
import Control.Arrow
import Data.Trie as Trie
import Data.ByteString
import Control.Monad.State (execState, State)

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


update :: a -> State a m -> a
update = flip execState

-- http://www.reddit.com/r/haskell/comments/1nwetz/lenses_that_work_with_arrows/cd2w5f1
-- https://gist.github.com/fizbin/7217274
(<~~) :: (Arrow ar) => ASetter s t a b -> ar s b -> ar s t
setter <~~ arrval = 
  proc x -> do
    bval <- arrval -< x
    returnA -< x & setter .~ bval
