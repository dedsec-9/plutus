{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import           API                         (API, PrivateKey, PublicKey, RawHtml (..))
import           Control.Monad.Except        (ExceptT)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (LoggingT, MonadLogger, logInfoN, runStderrLoggingT)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import qualified Crypto.Hash.SHA256          as SHA256
import           Data.Aeson                  (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Aeson                  as Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Lazy.UTF8   as BSLU
import qualified Data.ByteString.UTF8        as BSU
import           Data.Proxy                  (Proxy (Proxy))
import           Data.String                 as S
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           GHC.Generics                (Generic)
import           Network.Wai.Middleware.Cors (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import           Servant                     (Application, Handler (Handler), Server, ServerError, hoistServer, serve,
                                              serveDirectoryFileServer, throwError, (:<|>) ((:<|>)), (:>))

handlers :: FilePath -> Server API
handlers staticPath = createWallet :<|>
                      createWallet :<|>
                      testEndpoint :<|>
                      serveDirectoryFileServer staticPath

createWallet :: PrivateKey -> Handler PublicKey
createWallet privateKey = let publicKey = BSU.toString $ B16.encode $ SHA256.hash $ BSU.fromString privateKey in
                          pure publicKey

app :: FilePath -> Application
app staticPath =
  cors (const $ Just policy) $ serve (Proxy @API) (handlers staticPath)
  where
    policy =
      simpleCorsResourcePolicy

initializeApplication :: FilePath -> IO Application
initializeApplication staticPath = pure $ app staticPath

testEndpoint :: Handler RawHtml
testEndpoint = return $ RawHtml $ BSL.fromStrict "<html><head><title>Test page</title></head><body><h1>It works!</h1></body></html>"
