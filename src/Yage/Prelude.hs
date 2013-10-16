module Yage.Prelude
    ( module CorePrelude
    , io
    , traceShow', ioTime, printIOTime

    -- list functions
    , splitEvery
    , offset0

    , module Debug.Trace
    , module Text.Format
    , module Text.Show
    , Prelude.show
    ) where

import qualified   Prelude

import CorePrelude
import Text.Printf
import System.CPUTime
import Debug.Trace
import Text.Format
import Text.Show
import Foreign.Ptr

io :: (MonadIO m) => IO a -> m a
io = liftIO

traceShow' :: Show a => a -> a
traceShow' a = traceShow a a

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
    (first,rest) = Prelude.splitAt n list

-- stolen from: Graphics-GLUtil-BufferObjects
-- |A zero-offset 'Ptr'.
offset0 :: Ptr a
offset0 = offsetPtr 0

-- |Produce a 'Ptr' value to be used as an offset of the given number
-- of bytes.
offsetPtr :: Int -> Ptr a
offsetPtr = wordPtrToPtr . fromIntegral