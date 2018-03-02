{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module FileOperation where

import ArtifactTree
import Repository
import VersionTree
import Platform

import Data.Aeson.Text (encodeToTextBuilder)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy (unpack)
import System.FilePath
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import System.IO.Unsafe

versionTreeFile :: FilePath
versionTreeFile = "versionTree.json"

artifactTreeFile :: FilePath
artifactTreeFile = "artifactTree.json"

platformDBFile :: FilePath
platformDBFile = "platforms.json"

deploymentRulesFile :: FilePath
deploymentRulesFile = "deploymentRules.json"

{-# NOINLINE loadRepositoryFromFile #-} 
loadRepositoryFromFile :: Repository
loadRepositoryFromFile = unsafePerformIO $ fmap (fromJust . JSON.decode) $ B.readFile versionTreeFile
{-loadVersionTreeFromFile :: VersionTree-}
{-loadVersionTreeFromFile = fromJust $ JSON.decode [litFile|versionTree.json|]-}

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

instance SaveToFile Repository where
    saveToFile vDocTree = do
      withFile versionTreeFile WriteMode $ \h -> System.IO.hPutStr h (v2s $ JSON.toJSON vDocTree)

instance SaveToFile PlatformDB where
    saveToFile db = do
      withFile platformDBFile WriteMode $ \h -> System.IO.hPutStr h (v2s $ JSON.toJSON db)

instance SaveToFile DeploymentRules where
    saveToFile rules = do
      withFile deploymentRulesFile WriteMode $ \h -> System.IO.hPutStr h (v2s $ JSON.toJSON rules)
  
