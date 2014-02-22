{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where
import Test.Hspec (hspec, describe)
import MathNormalTest
import VectorInterleaveTest

main :: IO ()
main = do
    hspec $ do
        describe "calculate correct normals" $ do
            normalCalculation

        describe "interleave vector" $ do
            interleaveVectors
