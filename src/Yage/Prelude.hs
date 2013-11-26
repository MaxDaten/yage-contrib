module Yage.Prelude
    ( module CorePrelude
    , io, pass
    , traceShow', traceShowS, traceShowS', ioTime, printIOTime

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
    , module Lens
    , module Prelude
    ) where

import qualified Prelude                   as Prelude

import           Yage.Lens                 as Lens hiding ((<.>))

import           CorePrelude
import           Data.List                 (zip)
import           Debug.Trace
import           Filesystem.Path.CurrentOS as FilePath (decodeString,
                                                        encodeString)
import           Foreign.Ptr
import           System.CPUTime
import           Text.Format
import           Text.Printf
import           Text.Show

io :: (MonadIO m) => IO a -> m a
io = liftIO

pass :: IO ()
pass = return ()

traceShow' :: Show a => a -> a
traceShow' a = traceShow a a

traceShowS :: Show a => ShowS -> a -> a
traceShowS sf a = traceShow (sf $ show a) a

traceShowS' :: Show a => String -> a -> a
traceShowS' msg = traceShowS (msg Prelude.++)

ioTime :: MonadIO m => m a -> m (a, Double)
ioTime action = do
    start <- io $! getCPUTime
    v <- action
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


-- | flipped version of zip
piz = flip zip

infixl 4 <$$>
-- | infix operator to apply a function f to both values of a tuple
(<$$>) :: (a -> b) -> (a, a) -> (b, b)
f <$$> (x,y) = (f x, f y)

