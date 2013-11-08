module Yage.Prelude
    ( module CorePrelude
    , io, pass
    , traceShow', traceShowS, ioTime, printIOTime

    -- list functions
    , splitEvery
    , offset0

    , eqType, descending

    , (<?), (?>)
    , piz, (<$$>)
    , isLeft, isRight

    , module Debug.Trace
    , module Text.Format
    , module Text.Show
    , module FilePath
    , module P
    ) where

import qualified Prelude as P

import Data.Typeable
import CorePrelude
import Text.Printf
import System.CPUTime
import Debug.Trace
import Text.Format
import Text.Show
import Foreign.Ptr
import Data.List (zip)
import Filesystem.Path.CurrentOS as FilePath (encodeString, decodeString)

io :: (MonadIO m) => IO a -> m a
io = liftIO

pass :: IO ()
pass = return ()

traceShow' :: Show a => a -> a
traceShow' a = traceShow a a

traceShowS :: Show a => ShowS -> a -> a
traceShowS sf a = traceShow (sf $ show a) a

ioTime :: MonadIO m => m a -> m (a, Double)
ioTime op = do
    start <- io $! getCPUTime
    v <- op
    end <- io $! getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return $! (v, diff)


printIOTime :: MonadIO m => m a -> m a
printIOTime f = do
    (res, t) <- ioTime f
    io $! printf "Computation time: %0.5f sec\n" t
    return res

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = P.splitAt n list

-- stolen from: Graphics-GLUtil-BufferObjects
-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral

eqType :: (Typeable r, Typeable t) => r -> t -> Bool
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


piz = flip zip

(<$$>) :: (a -> b) -> (a, a) -> (b, b)
f <$$> (x,y) = (f x, f y) 

