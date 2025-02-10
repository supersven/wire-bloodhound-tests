{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import PutMapping
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "OpenSearch 1.3" $ do
    PutMapping.tests "https://localhost:7201"
  describe "ElasticSearch 6" $ do
    PutMapping.tests "https://localhost:7200"
