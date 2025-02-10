{-# LANGUAGE OverloadedStrings #-}

module PutMapping (tests) where

import Control.Monad.IO.Class
import Data.Aeson as Aeson
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import Lib
import Network.HTTP.Client
import Network.HTTP.Types
import Test.Hspec
import Test.QuickCheck (generate)

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

  describe "putNamedMapping" $ do
    it "can put our mapping" $ do
      reply <- runBH esUrl $ do
        idx :: ES.IndexName <- liftIO $ generate genIndexName
        createIndexWith [ES.AnalysisSetting analysisSettings] idx
        ES.putNamedMapping idx (ES.MappingName "_doc") indexMapping
      ES.isSuccess reply `shouldBe` True

  describe "putMapping" $ do
    it "can put our mapping" $ do
      reply <- runBH esUrl $ do
        idx :: ES.IndexName <- liftIO $ generate genIndexName
        createIndexWith [ES.AnalysisSetting analysisSettings] idx
        ES.putMapping idx indexMapping
      ES.isSuccess reply `shouldBe` True

analysisSettings :: ES.Analysis
analysisSettings =
  let analyzerDef =
        Map.fromList
          [ ("prefix_index", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "edge_ngram_1_30"] []),
            ("prefix_search", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "truncate_30"] [])
          ]
      filterDef =
        Map.fromList
          [ ("edge_ngram_1_30", ES.TokenFilterDefinitionEdgeNgram (ES.NgramFilter 1 30) Nothing),
            ("truncate_30", ES.TokenFilterTruncate 30)
          ]
   in ES.Analysis analyzerDef mempty filterDef mempty

data MappingProperty = MappingProperty
  { mpType :: MappingPropertyType,
    mpStore :: Bool,
    mpIndex :: Bool,
    mpAnalyzer :: Maybe Text.Text,
    mpFields :: Map.Map Text.Text MappingField
  }

data MappingField = MappingField
  { mfType :: MappingPropertyType,
    mfAnalyzer :: Maybe Text.Text,
    mfSearchAnalyzer :: Maybe Text.Text
  }

data MappingPropertyType = MPText | MPKeyword | MPByte | MPDate
  deriving (Eq)

instance ToJSON MappingProperty where
  toJSON mp =
    object
      ( [ "type" .= mpType mp,
          "store" .= mpStore mp,
          "index" .= mpIndex mp
        ]
          <> ["analyzer" .= mpAnalyzer mp | isJust $ mpAnalyzer mp]
          <> ["fields" .= mpFields mp | not . Map.null $ mpFields mp]
      )

instance ToJSON MappingPropertyType where
  toJSON MPText = Aeson.String "text"
  toJSON MPKeyword = Aeson.String "keyword"
  toJSON MPByte = Aeson.String "byte"
  toJSON MPDate = Aeson.String "date"

instance ToJSON MappingField where
  toJSON mf =
    object $
      ["type" .= mfType mf]
        <> ["analyzer" .= mfAnalyzer mf | isJust (mfAnalyzer mf)]
        <> ["search_analyzer" .= mfSearchAnalyzer mf | isJust (mfSearchAnalyzer mf)]

indexMapping :: Value
indexMapping =
  object
    [ "dynamic" .= False,
      "properties"
        .= object
          [ "normalized" -- normalized user name
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList [("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search"))]
                },
            "name"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "handle"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList
                      [ ("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search")),
                        ("keyword", MappingField MPKeyword Nothing Nothing)
                      ]
                },
            "email"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList
                      [ ("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search")),
                        ("keyword", MappingField MPKeyword Nothing Nothing)
                      ]
                },
            "team"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "accent_id"
              .= MappingProperty
                { mpType = MPByte,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "account_status"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "saml_idp"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "managed_by"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "created_at"
              .= MappingProperty
                { mpType = MPDate,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "role"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            -- TODO: Do we need to check this?
            --            searchVisibilityInboundFieldName
            --              .= MappingProperty
            --                { mpType = MPKeyword,
            --                  mpStore = False,
            --                  mpIndex = True,
            --                  mpAnalyzer = Nothing,
            --                  mpFields = mempty
            --                },
            "scim_external_id"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "sso"
              .= object
                [ "type" .= Aeson.String "nested",
                  "properties"
                    .= object
                      [ "issuer"
                          .= MappingProperty
                            { mpType = MPKeyword,
                              mpStore = False,
                              mpIndex = False,
                              mpAnalyzer = Nothing,
                              mpFields = mempty
                            },
                        "nameid"
                          .= MappingProperty
                            { mpType = MPKeyword,
                              mpStore = False,
                              mpIndex = False,
                              mpAnalyzer = Nothing,
                              mpFields = mempty
                            }
                      ]
                ],
            "email_unvalidated"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                }
          ]
    ]
