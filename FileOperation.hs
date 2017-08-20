{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module FileOperation where

import ArtifactTree
import VersionTree
import Platform

import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (unpack)
import System.FilePath
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import Data.Attoparsec
import Data.Attoparsec.Char8
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as B
import TH (litFile)
import Data.Maybe

versionTreeFile :: FilePath
versionTreeFile = "versionTree.json"

artifactTreeFile :: FilePath
artifactTreeFile = "artifactTree.json"

platformDBFile :: FilePath
platformDBFile = "platforms.json"

deploymentRulesFile :: FilePath
deploymentRulesFile = "deploymentRules.json"

{-loadVersionTreeFromFile :: IO (Maybe VersionTree)-}
{-loadVersionTreeFromFile = fmap JSON.decode $ B.readFile versionTreeFile-}
loadVersionTreeFromFile :: VersionTree
loadVersionTreeFromFile = fromJust $ JSON.decode $ [litFile|versionTree.json|]

v2s :: JSON.Value -> String
v2s = unpack . toLazyText . encodeToTextBuilder

class SaveToFile a where
    saveToFile :: a -> IO ()
    
instance SaveToFile ArtifactTree where
    saveToFile aTree = do
      withFile artifactTreeFile WriteMode $ \h -> System.IO.hPutStr h (v2s $ JSON.toJSON aTree)

instance SaveToFile VersionTree where
    saveToFile vTree = do
      withFile versionTreeFile WriteMode $ \h -> System.IO.hPutStr h (v2s $ JSON.toJSON vTree)

instance SaveToFile PlatformDB where
    saveToFile db = do
      withFile platformDBFile WriteMode $ \h -> System.IO.hPutStr h (v2s $ JSON.toJSON db)

instance SaveToFile DeploymentRules where
    saveToFile rules = do
      withFile deploymentRulesFile WriteMode $ \h -> System.IO.hPutStr h (v2s $ JSON.toJSON rules)
  
