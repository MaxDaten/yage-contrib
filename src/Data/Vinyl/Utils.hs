{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Vinyl.Utils where

import Yage.Prelude hiding (Identity)

import Data.Vinyl
import Data.Vinyl.Idiom.Identity


-- | extracts field values from a homogenous record with fields in a
-- generic functor
recToList :: FoldRec (Rec fs g) (g t) => Rec fs g -> [g t]
recToList = foldRec (\e a -> [e] ++ a) []


-- | extracts field values from a homogenous record with fields in the
-- Identity functor and resolves (unpacks) the values from the functor
recToList' :: FoldRec (Rec fs Identity) (Identity t) => Rec fs Identity -> [t]
recToList' = map runIdentity . recToList


-- morphism between records
class RMap r s a b where
    rmap :: (a -> b) -> r -> s

instance RMap (Rec '[] f) (Rec '[] f) a b where
    rmap _ _ = RNil


-- instance FoldRec (Rec fs g) (g t) => FoldRec (Rec ((s ::: t) ': fs) g) (g t) where
instance ( Applicative f, RMap (Rec fs f) (Rec fs f) (f a) (f b) ) => RMap (Rec ((s ::: a) ': fs) f) (Rec ((s ::: b) ': fs) f) (f a) (f b) where
    rmap f (x :& xs) = (Field::s:::b) <-: f x <+> rmap f xs

{--
type Pos = "pos" ::: Int
type Tex = "tex" ::: Int
type PTRec = PlainRec [Pos, Tex]

ptRec :: PTRec
ptRec = Field =: 10 <+> Field =: 20

ptRecStr :: PTRec -> PlainRec ["position" ::: String, "texture" ::: String]
ptRecStr = rmap f
    where 
    f :: Identity Int -> Identity String
    f = fmap (show . (+3))
--}
