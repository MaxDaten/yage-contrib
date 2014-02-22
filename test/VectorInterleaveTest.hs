{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module VectorInterleaveTest where

import Test.Hspec
import Test.QuickCheck

import Data.Vector.Storable.Extras
import qualified Data.Vector.Storable as V

a :: V.Vector Int
a = V.fromList [1, 2, 3]

b :: V.Vector Int
b = V.fromList [4, 5, 6]

c :: V.Vector Int
c = V.fromList [7, 8, 9]

iab :: V.Vector Int
iab = V.fromList [1, 4, 2, 5, 3, 6]


iabc :: V.Vector Int
iabc = V.fromList [1, 4, 7, 2, 5, 8, 3, 6, 9]


interleaveVectors :: Spec
interleaveVectors = do
    describe "a way to interleave multiple vectors" $ do
        
        it "interleaves two vectors" $ do
            let ab = interleave [a, b]
            ab `shouldBe` iab

        it "interleaves three vectors" $ do
            let abc = interleave [a, b, c]
            abc `shouldBe` iabc

        it "interleaves two singleton vectors" $ do
            let ab = interleave $ [V.singleton 1, V.singleton 2] :: V.Vector Int
            ab `shouldBe` (V.fromList [1, 2] :: V.Vector Int)

        it "interleaves two empty vectors without an error" $ do
            let ab = interleave $ [V.empty, V.empty]
            ab `shouldBe` (V.empty :: V.Vector Int)


