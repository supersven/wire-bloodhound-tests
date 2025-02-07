{-# LANGUAGE OverloadedStrings #-}

-- | Test helper functions and imports (copies) from wire-server.
module Lib where

import Control.Monad.IO.Class
import Data.Text
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL (opensslManagerSettings)
import OpenSSL.Session (SSLOption (..))
import OpenSSL.Session qualified as SSL
import Test.Hspec
import Test.QuickCheck
import UnliftIO.Directory

genIndexName :: Gen ES.IndexName
genIndexName =
  ES.IndexName . Text.pack
    <$> (listOf (elements ['a' .. 'z']) `suchThat` ((< 255) . Prelude.length))

--- Blow is wire-server code

runBH :: (MonadIO m, HasCallStack) => Text -> ES.BH m a -> m a
runBH esURL action = do
  mgr <- liftIO $ initHttpManagerWithTLSConfig True Nothing
  let bEnv = mkBHEnv esURL mgr
  ES.runBH bEnv action

initHttpManagerWithTLSConfig :: Bool -> Maybe FilePath -> IO Manager
initHttpManagerWithTLSConfig skipTlsVerify mCustomCa = do
  -- See Note [SSL context]
  ctx <- SSL.context
  SSL.contextAddOption ctx SSL_OP_NO_SSLv2
  SSL.contextAddOption ctx SSL_OP_NO_SSLv3
  SSL.contextSetCiphers ctx "HIGH"
  if skipTlsVerify
    then SSL.contextSetVerificationMode ctx SSL.VerifyNone
    else
      SSL.contextSetVerificationMode ctx $
        SSL.VerifyPeer True True Nothing
  case mCustomCa of
    Nothing -> SSL.contextSetDefaultVerifyPaths ctx
    Just customCa -> do
      filePath <- canonicalizePath customCa
      SSL.contextSetCAFile ctx filePath
  -- Unfortunately, there are quite some AWS services we talk to
  -- (e.g. SES, Dynamo) that still only support TLSv1.
  -- Ideally: SSL.contextAddOption ctx SSL_OP_NO_TLSv1
  newManager
    (opensslManagerSettings (pure ctx))
      { managerConnCount = 1024,
        managerIdleConnectionCount = 4096,
        managerResponseTimeout = responseTimeoutMicro 10000000
      }

mkBHEnv :: Text -> Manager -> ES.BHEnv
mkBHEnv url mgr = do
  (ES.mkBHEnv (ES.Server url) mgr) {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername "elastic") (ES.EsPassword "changeme")}
