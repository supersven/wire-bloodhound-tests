{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import Lib
import Network.HTTP.Client
import Network.HTTP.Types
import Test.Hspec
import Test.QuickCheck (generate)

main :: IO ()
main = hspec $ do
  describe "OpenSearch 1.3" $ tests "https://localhost:7201"
  describe "ElasticSearch 6" $ tests "https://localhost:7200"

tests :: Text.Text -> Spec
tests esUrl = do
  describe "createIndex" $ do
    it "can create indices" $ do
      reply <- runBH esUrl $ do
        idx :: ES.IndexName <- liftIO $ generate genIndexName
        ES.createIndex ES.defaultIndexSettings idx
      ES.isSuccess reply `shouldBe` True
      -- TODO: ES.isCreated expects statusCode 201 and thus fails!
      -- ES.isCreated reply `shouldBe` True
      (statusCode . responseStatus) reply `shouldBe` 200
