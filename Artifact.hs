{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies #-}

module Artifact where

import Version
import VersionNumber
import MaturityLevel
import Document
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HashMap

type BranchName = String
type Timestamp = Integer


data Branch = Branch BranchName Version DocumentOrDirectory
instance Eq Branch where
    (Branch nameA versionA _ ) == (Branch nameB versionB _ )                = (nameA == nameB) && (versionA == versionB)
    _ == _                                                                  = False
instance Show Branch where
    show (Branch branchName version contents ) = ("Branch '" ++ branchName ++ "', Version " ++ (versionToString version) ++ ", " ++ (show contents) ++ "")

data Snapshot = Snapshot Timestamp Version DocumentOrDirectory 
data Snapshot2 = Snapshot2 Timestamp DocumentOrDirectory deriving (Show, Eq)

instance Eq Snapshot where
    (Snapshot timestampA versionA _ ) == (Snapshot timestampB versionB _ )  = (timestampA == timestampB) && (versionA == versionB)
    _ == _                                                                  = False

instance Show Snapshot where
    show (Snapshot timestamp version contents ) = ("Snapshot taken at " ++ (show timestamp) ++ ", Version " ++ (versionToString version) ++ ", " ++ (show contents) ++ "")

newtype Artifact = Artifact (Either Branch Snapshot) deriving (Show, Eq)

liftBranch :: Branch-> Artifact
liftBranch = Artifact . Left

liftSnapshot :: Snapshot -> Artifact
liftSnapshot = Artifact . Right

instance JSON.ToJSON Branch where
    toJSON (Branch branchName version document) = JSON.object [ "branch" JSON..= JSON.object [ 
      "version"  JSON..= JSON.toJSON version,
      "name" JSON..= branchName,
      "artifact"  JSON..= JSON.toJSON document ]]
    
instance JSON.ToJSON Snapshot where        
    toJSON (Snapshot timestamp version document) = JSON.object [ "snapshot" JSON..= JSON.object [ 
      "version"  JSON..= JSON.toJSON version,
      "timestamp" JSON..= timestamp,
      "artifact"  JSON..= JSON.toJSON document ]]
      
instance JSON.ToJSON Snapshot2 where        
    toJSON (Snapshot2 timestamp document) = JSON.object [ "snapshot" JSON..= JSON.object [ 
      "timestamp" JSON..= timestamp,
      "artifact"  JSON..= JSON.toJSON document ]]

instance JSON.ToJSON Artifact where
  toJSON (Artifact (Left d))  = JSON.toJSON d
  toJSON (Artifact (Right d)) = JSON.toJSON d

-- FromJSON 
instance JSON.FromJSON Branch where
  parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "branch" v
    where parser (JSON.Object v') = Branch <$> v' JSON..: "name"
                                             <*> v' JSON..: "version"
                                             <*> v' JSON..: "artifact"
          parser _                = mzero
  parseJSON _          = mzero
  
instance JSON.FromJSON Snapshot where
  parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "snapshot" v
    where parser (JSON.Object v') = Snapshot <$> v' JSON..: "timestamp"
                                             <*> v' JSON..: "version"
                                             <*> v' JSON..: "artifact"
          parser _                = mzero
  parseJSON _          = mzero
  
instance JSON.FromJSON Snapshot2 where
  parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "snapshot" v
    where parser (JSON.Object v') = Snapshot2 <$> v' JSON..: "timestamp"
                                             <*> v' JSON..: "artifact"
          parser _                = mzero
  parseJSON _          = mzero

instance JSON.FromJSON Artifact where
  parseJSON json = (liftBranch <$> JSON.parseJSON json) <|> (liftSnapshot <$> JSON.parseJSON json)

type ArtifactList = [Artifact]

-- artifactToString :: Artifact -> String
-- artifactToString (Branch branchName version _) = (branchName ++ " (" ++ ( versionToString version ) ++ ")")
-- artifactToString (Snapshot timestamp version _) = (versionToString version)

-- artifactToVersion :: Artifact -> Version
-- artifactToVersion (Branch branchName version _) = version
-- artifactToVersion (Snapshot timestamp version _) = version

-- getArtifactVersion = artifactToVersion

-- artifactListToVersionList :: ArtifactList -> VersionList
-- artifactListToVersionList [] = []
-- artifactListToVersionList (x:xs) = (artifactListToVersionList xs) ++ [artifactToVersion x]

-- artifactListToString :: ArtifactList -> [String]
-- artifactListToString [] = []
-- artifactListToString (x:xs) = (artifactListToString xs) ++ [artifactToString x]

-- artifactToDocument :: Artifact -> DocumentOrDirectory
-- artifactToDocument (Branch _ _ document) = document
-- artifactToDocument (Snapshot _ _ document) = document

-- getArtifactDocument = artifactToDocument
-- getArtifactContents = artifactToDocument

data AllowedChanges = Any
                    | None
                    deriving (Show)

class DetectAllowedChanges entity where
    detectAllowedChanges :: entity -> AllowedChanges

instance DetectAllowedChanges VersionCompound where
    detectAllowedChanges NumberPlaceholder  = Any
    detectAllowedChanges (Number n)         = None 

instance DetectAllowedChanges VersionNumber where
    detectAllowedChanges (VersionCompound vc)  = detectAllowedChanges vc
    detectAllowedChanges (VersionNumber vc vn) = detectAllowedChanges vn 
    
instance DetectAllowedChanges Version where
    detectAllowedChanges (Version num) = detectAllowedChanges num
    detectAllowedChanges (MaturityVersion level num) = detectAllowedChanges num

-- instance DetectAllowedChanges Artifact where
--     detectAllowedChanges (Branch branchName version _ ) = detectAllowedChanges version
--     detectAllowedChanges (Snapshot timestamp version _ ) = detectAllowedChanges version

allowedChangesToString :: AllowedChanges -> String
allowedChangesToString None = "None"
allowedChangesToString Any = "Any"

-- artifactHasVersion :: Artifact -> Version -> Bool
-- artifactHasVersion (Branch _ v1 _) v2 = (v1 == v2)
-- artifactHasVersion (Snapshot _ v1 _) v2 = (v1 == v2)

-- getArtifactTimestamp :: Artifact -> Timestamp
-- getArtifactTimestamp (Snapshot timestamp _ _) = timestamp
-- getArtifactTimestamp (Branch _ _ _) = 0

-- getArtifactName :: Artifact -> String
-- getArtifactName (Snapshot _ _ _) = ""
-- getArtifactName (Branch name _ _) = name

data ArtifactTreeOperation = ArtifactTreeEdit Artifact DocumentOrDirectory
                    -- | Save DocumentName Artifact -- Use CreateSnapshot instead
                    -- | Copy DocumentName Artifact -- Use CreateBranch instead
                       | ArtifactTreeCreateSnapshot Artifact 
                       | ArtifactTreeCreateBranch Artifact BranchName
                    -- | Share   
                       deriving (Show)

data DocumentChangeType = DocumentChangeAdd
                        | DocumentChangeDelete
                        | DocumentChangeUpdate
                        | DocumentChangeRename
                        deriving (Show)
                        
-- type Change = DocumentContent -> DocumentContent

data ChangeGranularity = SaveGranularity
                    -- | CompilationGranularity
                    -- | RevisionGranularity
                    -- | ContinuousIntegrationBuildGranularity
                       | SnapshotGranularity
                    -- | DeploymentGranularity
                       | MinorReleaseGranularity
                       | MajorReleaseGranularity

detectChangeType :: DocumentOrDirectory -> DocumentOrDirectory -> DocumentChangeType
-- detectChangeType ... added lines ... = Add
-- detectChangeType ... removed lines ... = Delete
-- detectChangeType ... changed lines ... = Update
-- detectChangeType ... renamed ... = Rename
detectChangeType = undefined

data BranchType = Mainline
                | Feature
                | Experimental
                | Support
                | Release
                deriving (Show)
                
-- TODO: incorporate branch types

-- EXAMPLES:--

snapshot1 :: Snapshot
snapshot1 = Snapshot 12372 ( MaturityVersion Dev ( VersionCompound ( Number 10 ) ) ) ( liftDocument doc1 )

snapshot2 :: Snapshot2
snapshot2 = Snapshot2 12372  ( liftDocument doc1 )

branch1 :: Branch
branch1 = Branch "branch3" ( Version ( VersionCompound NumberPlaceholder ) ) ( liftDocument doc2 )

artifact1 :: Artifact
artifact1 = liftSnapshot $ snapshot1

artifact2 :: Artifact
artifact2 = liftBranch $ branch1

-- artifact3 :: Artifact
-- artifact3 = Branch "branch22" ( Version ( VersionCompound NumberPlaceholder ) ) ( Document "document22" "content_branch22" )

-- artifact4 :: Artifact
-- artifact4 = Snapshot 12372 ( Version ( VersionCompound ( Number 11 ) ) ) ( Document "document1" "content11" )

