{-# LANGUAGE OverloadedStrings #-}

module IndexDocument where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import GHC.Natural
import Lib
import Test.Hspec
import Test.QuickCheck

tests :: Text.Text -> Spec
tests esUrl = do
  describe "indexDocument" $ do
    it "should index documents" $ do
      idx :: ES.IndexName <- liftIO $ generate genIndexName
      reply <- runBH esUrl $ do
        createIndexWith [] idx
        ES.putNamedMapping idx migrationMappingName migrationIndexMapping
      ES.isSuccess reply `shouldBe` True

      let v = MigrationVersion 6
          docId = ES.DocId . Text.pack . show $ migrationVersion v
      persistResponse <- runBH esUrl $ ES.indexDocument idx migrationMappingName ES.defaultIndexDocumentSettings v docId
      ES.isCreated persistResponse `shouldBe` True

newtype MigrationVersion = MigrationVersion {migrationVersion :: Natural}
  deriving (Show, Eq, Ord)

instance ToJSON MigrationVersion where
  toJSON (MigrationVersion v) = object ["migration_version" .= v]

instance FromJSON MigrationVersion where
  parseJSON = withObject "MigrationVersion" $ \o -> MigrationVersion <$> o .: "migration_version"

migrationMappingName :: ES.MappingName
migrationMappingName = ES.MappingName "wire_brig_migrations"

migrationIndexMapping :: Value
migrationIndexMapping =
  object
    [ "properties"
        .= object
          ["migration_version" .= object ["index" .= True, "type" .= ("integer" :: Text.Text)]]
    ]
