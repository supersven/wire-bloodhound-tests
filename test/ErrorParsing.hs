{-# LANGUAGE OverloadedStrings #-}

module ErrorParsing where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Either
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import Debug.Trace
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
      traceM $ "Error : " ++ show parsed
      parsed `shouldSatisfy` isLeft
      ES.isCreated response `shouldBe` False
