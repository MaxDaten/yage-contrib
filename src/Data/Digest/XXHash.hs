{-# LANGUAGE PackageImports #-}
module Data.Digest.XXHash
    ( module X
    , module Data.Digest.XXHash
    ) where

import Yage.Prelude

import Data.ByteString as BS
import "xxhash" Data.Digest.XXHash as X

xxHashFile :: FilePath -> IO XXHash
xxHashFile path = X.xxHash' <$> BS.readFile (encodeString path)

