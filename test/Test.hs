{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.Hspec
import Test.DocTest (doctest)

import qualified Mock
import qualified Property

hspecTests :: Spec
hspecTests = do
    Mock.mockHspec

propertyTests = do
    pure ()

main :: IO ()
main = do
    print "hspec tests"
    hspec hspecTests

    print "doctests"
    doctest ["-isrc", "src"]

    print "quickcheck"
    propertyTests