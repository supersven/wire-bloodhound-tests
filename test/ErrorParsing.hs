{-# LANGUAGE OverloadedStrings #-}

module ErrorParsing where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Either
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import Lib
import Test.Hspec
import Test.QuickCheck

tests :: Text.Text -> Spec
tests esUrl = do
  describe "ErrorParsing" $ do
    it "should be able to parse errors" $ do
      idx :: ES.IndexName <- liftIO $ generate genIndexName

      let query = ES.QueryMatchQuery $ ES.mkMatchQuery (ES.FieldName "_all") (ES.QueryString "haskell")
      let search = (ES.mkSearch (Just query) Nothing) {ES.source = Just ES.NoSource}

      response <- runBH esUrl $ ES.searchByIndex idx search
      parsed :: Either ES.EsError (ES.SearchResult Value) <- ES.parseEsResponse response
      parsed `shouldSatisfy` isLeft
      ES.isCreated response `shouldBe` False

    it "should parse OpenSearch 1.3 responses" $ do
      -- This is a real error response for a version conflict
      let mbJson = decode "{\"took\":9,\"timed_out\":false,\"total\":3,\"updated\":1,\"deleted\":0,\"batches\":1,\"version_conflicts\":2,\"noops\":0,\"retries\":{\"bulk\":0,\"search\":0},\"throttled_millis\":0,\"requests_per_second\":-1.0,\"throttled_until_millis\":0,\"failures\":[{\"index\":\"directory_test\",\"type\":\"_doc\",\"id\":\"9fda4188-2afd-490d-8796-e023df61a4e9\",\"cause\":{\"type\":\"version_conflict_engine_exception\",\"reason\":\"[9fda4188-2afd-490d-8796-e023df61a4e9]: version conflict, required seqNo [11], primary term [1]. current document has seqNo [16] and primary term [1]\",\"index\":\"directory_test\",\"shard\":\"0\",\"index_uuid\":\"Y3RpVY_DQEW9ULn8oGulrg\"},\"status\":409},{\"index\":\"directory_test\",\"type\":\"_doc\",\"id\":\"d70b631d-966a-4951-a94c-35ddc210f28a\",\"cause\":{\"type\":\"version_conflict_engine_exception\",\"reason\":\"[d70b631d-966a-4951-a94c-35ddc210f28a]: version conflict, required seqNo [13], primary term [1]. current document has seqNo [15] and primary term [1]\",\"index\":\"directory_test\",\"shard\":\"0\",\"index_uuid\":\"Y3RpVY_DQEW9ULn8oGulrg\"},\"status\":409}]}"
      mbJson `shouldBe` Just (ES.EsError 409 "[9fda4188-2afd-490d-8796-e023df61a4e9]: version conflict, required seqNo [11], primary term [1]. current document has seqNo [16] and primary term [1]")
