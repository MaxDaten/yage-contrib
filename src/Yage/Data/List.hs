{-# LANGUAGE PackageImports #-}
module Yage.Data.List 
    ( module List
    , module Yage.Data.List
    ) where

import Prelude (flip)
import Data.List as List
import Data.List.Split as List


--splitEvery :: Int -> [a] -> [[a]]
--splitEvery _ [] = []
--splitEvery n list = frst : (splitEvery n rest)
--  where
--    (frst,rest) = Prelude.splitAt n list


shift :: [a] -> [a]
shift list = last list : init list


-- | flipped version of zip
piz :: [b] -> [a] -> [(a, b)]
piz = flip zip
