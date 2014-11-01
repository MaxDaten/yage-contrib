{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans -fno-warn-missing-fields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Yage.Prelude
    ( module ClassyPrelude
    , io, pass
    , traceShowS, traceShowS', ioTime, printIOTime, traceWith
    , printTF

    -- list functions
    , zipWithTF
    , offset0

    , eqType, descending

    , (<?), (?>)
    , isLeft, isRight
    , Identity()

    , qStr

    , module Text.Show
    , module TF
    , module TextRead
    , module FilePath
    , module DeepSeq
    , module Default
    , module Prelude
    ) where

import qualified Prelude                   as Prelude
import           ClassyPrelude

import           Data.Text.Format          as TF hiding ( print )
import           Data.Text.Format.Params   ( Params )
import qualified Data.Text.Format          as TF ( print, )
import           Data.Text.Read            as TextRead

import           Data.Typeable
import           Data.Data
import           Data.Traversable          as Trav
import           Data.Foldable             as Fold
import           Data.Functor.Identity
import           Data.Default              as Default

import           Control.DeepSeq           as DeepSeq
import           Control.DeepSeq.Generics  as DeepSeq


import           Filesystem.Path.CurrentOS as FilePath (decodeString,
                                                        encodeString)
import           Foreign.Ptr
import           System.CPUTime
import           Text.Printf
import           Text.Show
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

io :: (MonadIO m) => IO a -> m a
io = liftIO

pass :: IO ()
pass = return ()

traceShowS :: Show a => ShowS -> a -> a
traceShowS sf a = traceShow (sf $ show a) a

traceShowS' :: Show a => String -> a -> a
traceShowS' msg = traceShowS (msg Prelude.++)

traceWith :: Show b => (a -> b) -> a -> a
traceWith f a = traceShow (f a) a

printTF :: (MonadIO m, Params ps) => Format -> ps -> m ()
printTF = TF.print

-- | time a monadic action in seconds, the monadic value is strict evaluated
ioTime :: MonadIO m => m a -> m (a, Double)
ioTime action = do
    start <- io $! getCPUTime
    v <- action
    end <- v `seq` io $! getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return $! (v, diff)


printIOTime :: MonadIO m => m a -> m a
printIOTime f = do
    (res, t) <- ioTime f
    _ <- io $! printf "Computation time: %0.5f sec\n" t
    return res


-- stolen from: Graphics-GLUtil-BufferObjects
-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

eqType :: (Typeable r, Typeable t) => Proxy r -> Proxy t -> Bool
eqType r t = (typeOf r) == (typeOf t)


(?>) :: a -> Maybe a -> a
l ?> mr = maybe l id mr

(<?) :: Maybe a -> a -> a
(<?) = flip (?>)

descending :: (a -> a -> Ordering) -> (a -> a -> Ordering)
descending cmp = flip cmp

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False


isRight :: Either a b -> Bool
isRight (Right _)= True
isRight _        = False


zipWithTF :: (Traversable t, Foldable f) => (a -> b -> c) -> t a -> f b -> t c
zipWithTF g t f = snd (Trav.mapAccumL map_one (Fold.toList f) t)
  where map_one (x:xs) y = (xs, g y x)
        map_one _ _ = error "Yage.Prelude.zipWithTF"

qStr :: QuasiQuoter
qStr = QuasiQuoter { quoteExp = stringE }


deriving instance Data Zero
deriving instance Typeable Zero
deriving instance Data a => Data (Succ a)
deriving instance Typeable Succ
