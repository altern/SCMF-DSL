{-# LANGUAGE OverloadedStrings , DeriveFunctor,DeriveAnyClass, DeriveGeneric #-}

module Document where

import qualified Data.Aeson as JSON
-- import Data.Text
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HashMap
import Data.ByteString.Lazy.Char8 as BLS
import GHC.Generics (Generic)

type DocumentName = String
type DirectoryName = String
type DocumentContent = String

data Document = Document {name :: DocumentName, content :: DocumentContent } 
      deriving (Eq, Generic,  JSON.FromJSON, JSON.ToJSON)
data Directory = Directory DirectoryName [DocumentOrDirectory] deriving (Eq)
newtype DocumentOrDirectory = DocumentOrDirectory (Either Document Directory) deriving (Eq)

emptyDocument = Document "" "" 

class GetDocument a where 
  getDocumentName :: a -> String
  getDocumentContent :: a -> String

instance GetDocument Document where
  getDocumentName (Document name _) = name
  getDocumentContent (Document _ content) = content 

instance Show Document where
    show (Document name content) = "Document name: " ++ name ++ ", Content: " ++ content ++ ""
instance Show Directory where
    show (Directory dirName content ) = "Directory name: " ++ dirName ++ ", Content: " ++ (show content) ++ ""
instance Show DocumentOrDirectory where
    show ( DocumentOrDirectory (Left doc)) = show doc
    show ( DocumentOrDirectory (Right dir)) = show dir
                         
liftDocument :: Document -> DocumentOrDirectory
liftDocument = DocumentOrDirectory . Left
 
liftDirectory :: Directory -> DocumentOrDirectory
liftDirectory = DocumentOrDirectory . Right

editDocument :: Document -> DocumentOrDirectory -> DocumentOrDirectory
editDocument newDoc (DocumentOrDirectory (Left doc)) = DocumentOrDirectory (Left newDoc)

editDirectory :: Directory -> DocumentOrDirectory -> DocumentOrDirectory
editDirectory newDir (DocumentOrDirectory (Right dir)) = DocumentOrDirectory (Right newDir)

-- ToJSON
{-instance JSON.ToJSON Document where-}
  {-toJSON (Document name content) = JSON.object [ "document" JSON..= JSON.object [-}
    {-"name"    JSON..= name,-}
    {-"content" JSON..= content ]]-}
 
instance JSON.ToJSON Directory where
  toJSON (Directory name content) = JSON.object [ "directory" JSON..= JSON.object [
    "name"    JSON..= name,
    "content" JSON..= content ]]
 
instance JSON.ToJSON DocumentOrDirectory where
  toJSON (DocumentOrDirectory (Left d))  = JSON.toJSON d
  toJSON (DocumentOrDirectory (Right d)) = JSON.toJSON d
    
-- FromJSON 
{-instance JSON.FromJSON Document where-}
  {-parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "document" v-}
    {-where parser (JSON.Object v') = Document <$> v' JSON..: "name"-}
                                        {-<*> v' JSON..: "content"-}
          {-parser _           = mzero-}
  {-parseJSON _          = mzero-}

{-instance JSON.FromJSON Directory where-}
  {-parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "directory" v-}
    {-where parser (JSON.Object v') = Directory <$> v' JSON..: "name"-}
                                         {-<*> v' JSON..: "content"-}
          {-parser _           = mzero-}
  {-parseJSON _          = mzero-}

{-instance JSON.FromJSON DocumentOrDirectory where-}
  {-parseJSON json = (liftDocument <$> JSON.parseJSON json) <|> (liftDirectory <$> JSON.parseJSON json)-}

-- EXAMPLES --
doc1 :: Document
doc1 = Document "doc1" "content1"
doc2 :: Document
doc2 = Document "doc2" "content2"

dir1 :: Directory
dir1 = Directory "dir1" [DocumentOrDirectory (Left doc1)]
dir2 :: Directory
dir2 = Directory "dir2" [DocumentOrDirectory (Left doc2)]
dir3 :: Directory
dir3 = Directory "dir2" [DocumentOrDirectory (Left doc1), DocumentOrDirectory (Left doc2)]

jsonDoc1 :: String
jsonDoc1 = "{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}}"
jsonDoc2 :: String
jsonDoc2 = "{\"document\":{\"name\":\"doc2\",\"content\":\"content2\"}}"
jsonDir1 :: String
jsonDir1 = "{\"directory\":{\"name\":\"dir1\",\"content\":[{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}}]}}"

