module Yage.Prelude
    ( module CorePrelude
    , io
    , traceShow', ioTime, printIOTime

    -- list functions
    , splitEvery

    , module Debug.Trace
    , Prelude.show
    ) where

import qualified   Prelude

import CorePrelude
import Text.Printf
import System.CPUTime
import Debug.Trace

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