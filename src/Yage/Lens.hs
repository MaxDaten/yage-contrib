{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE Arrows #-}
module Yage.Lens (
      module Yage.Lens
    , module Lens
    , module BSLens
    ) where

import Prelude (id, const, flip)
import Data.Maybe

import Control.Arrow hiding ((<+>))
import Data.Trie as Trie
import Data.Vinyl
import Data.ByteString
import Data.ByteString.Lens as BSLens

import Control.Monad.State (execState, State)

import Control.Applicative (pure)

import Control.Lens as Lens

infixr 4 %?~, <<+>~


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
{-# INLINE update #-}


(<<+>~) ::Setting (->) s t (Rec el f as) (Rec el f (as ++ bs))
      -> Rec el f bs -> s -> t
(<<+>~) u v = u %~ (\v' -> v' <+> v)
{-# INLINE (<<+>~) #-}


-- http://www.reddit.com/r/haskell/comments/1nwetz/lenses_that_work_with_arrows/cd2w5f1
-- https://gist.github.com/fizbin/7217274
(<~~) :: (Arrow ar) => ASetter s t a b -> ar s b -> ar s t
setter <~~ arrval =
  proc x -> do
    bval <- arrval -< x
    returnA -< x & setter .~ bval
{-# INLINE (<~~) #-}
