{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module FileOperation where

import ArtifactTree
import VersionTree

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


versionTreeFile :: FilePath
versionTreeFile = "versionTree.json"

artifactTreeFile :: FilePath
artifactTreeFile = "artifactTree.json"

loadArtifactTreeFromFile :: IO B.ByteString
loadArtifactTreeFromFile = B.readFile versionTreeFile

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
  