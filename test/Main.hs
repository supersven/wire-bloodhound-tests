{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ErrorParsing
import IndexDocument
import PutMapping
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "OpenSearch 1.3" $ do
    PutMapping.tests "https://localhost:7201"
    IndexDocument.tests "https://localhost:7201"
    ErrorParsing.tests "https://localhost:7201"
  describe "ElasticSearch 6" $ do
    PutMapping.tests "https://localhost:7200"
    IndexDocument.tests "https://localhost:7200"
    ErrorParsing.tests "https://localhost:7200"
