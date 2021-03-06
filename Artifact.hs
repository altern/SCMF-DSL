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
--    _ == _                                                                  = False

instance Show Branch where
    show (Branch branchName version contents ) = ("Branch '" ++ branchName ++ "', Version " ++ (toString version) ++ ", " ++ (show contents) ++ "")

data Snapshot = Snapshot Timestamp Version DocumentOrDirectory 
data Snapshot2 = Snapshot2 Timestamp DocumentOrDirectory deriving (Show, Eq)

instance Eq Snapshot where
    (Snapshot timestampA versionA _ ) == (Snapshot timestampB versionB _ )  = (timestampA == timestampB) && (versionA == versionB)
--    _ == _                                                                  = False

instance Show Snapshot where
    show (Snapshot timestamp version contents ) = ("Snapshot taken at " ++ (show timestamp) ++ ", Version " ++ (toString version) ++ ", " ++ (show contents) ++ "")

newtype Artifact = Artifact (Either Branch Snapshot) deriving (Show, Eq)

liftBranch :: Branch-> Artifact
liftBranch = Artifact . Left

liftSnapshot :: Snapshot -> Artifact
liftSnapshot = Artifact . Right

{-instance JSON.ToJSON Branch where-}
    {-toJSON (Branch branchName version document) = JSON.object [ "branch" JSON..= JSON.object [ -}
      {-"version"  JSON..= JSON.toJSON version,-}
      {-"name" JSON..= branchName,-}
      {-"artifact"  JSON..= JSON.toJSON document ]]-}
    
{-instance JSON.ToJSON Snapshot where        -}
    {-toJSON (Snapshot timestamp version document) = JSON.object [ "snapshot" JSON..= JSON.object [ -}
      {-"version"  JSON..= JSON.toJSON version,-}
      {-"timestamp" JSON..= timestamp,-}
      {-"artifact"  JSON..= JSON.toJSON document ]]-}
      
{-instance JSON.ToJSON Snapshot2 where        -}
    {-toJSON (Snapshot2 timestamp document) = JSON.object [ "snapshot" JSON..= JSON.object [ -}
      {-"timestamp" JSON..= timestamp,-}
      {-"artifact"  JSON..= JSON.toJSON document ]]-}

{-instance JSON.ToJSON Artifact where-}
  {-toJSON (Artifact (Left d))  = JSON.toJSON d-}
  {-toJSON (Artifact (Right d)) = JSON.toJSON d-}

-- FromJSON 
{-instance JSON.FromJSON Branch where-}
  {-parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "branch" v-}
    {-where parser (JSON.Object v') = Branch <$> v' JSON..: "name"-}
                                             {-<*> v' JSON..: "version"-}
                                             {-<*> v' JSON..: "artifact"-}
          {-parser _                = mzero-}
  {-parseJSON _          = mzero-}
  
{-instance JSON.FromJSON Snapshot where-}
  {-parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "snapshot" v-}
    {-where parser (JSON.Object v') = Snapshot <$> v' JSON..: "timestamp"-}
                                             {-<*> v' JSON..: "version"-}
                                             {-<*> v' JSON..: "artifact"-}
          {-parser _                = mzero-}
  {-parseJSON _          = mzero-}
  
{-instance JSON.FromJSON Snapshot2 where-}
  {-parseJSON (JSON.Object v) = maybe mzero parser $ HashMap.lookup "snapshot" v-}
    {-where parser (JSON.Object v') = Snapshot2 <$> v' JSON..: "timestamp"-}
                                             {-<*> v' JSON..: "artifact"-}
          {-parser _                = mzero-}
  {-parseJSON _          = mzero-}

{-instance JSON.FromJSON Artifact where-}
  {-parseJSON json = (liftBranch <$> JSON.parseJSON json) <|> (liftSnapshot <$> JSON.parseJSON json)-}

type ArtifactList = [Artifact]

initialArtifact :: NumberOfDimensions -> Artifact
initialArtifact dim = Artifact ( Left ( Branch "trunk" ( Version $ createVersionNumberByNumberOfDimensions dim ) ( liftDocument $ Document "" "" ) ) )

class IsInitialArtifact a where
    isInitialArtifact :: a -> Bool

instance IsInitialArtifact Artifact where
    isInitialArtifact a = case a of 
        (Artifact ( Right ( Snapshot _ ( v ) _ ) ) ) -> isInitial v
        (Artifact ( Left  ( Branch "trunk" v _ ) ) ) -> isInitial v
        _ -> False

instance ToString Artifact where
    toString (Artifact (Left (Branch branchName version _))) = (branchName ++ " (" ++ ( toString version ) ++ ")")
    toString (Artifact (Right (Snapshot timestamp version _))) = (toString version)

artifactToVersion :: Artifact -> Version
artifactToVersion (Artifact (Left (Branch branchName version _))) = version
artifactToVersion (Artifact (Right (Snapshot timestamp version _))) = version

getArtifactVersion = artifactToVersion

artifactListToVersionList :: ArtifactList -> VersionList
artifactListToVersionList [] = []
artifactListToVersionList (x:xs) = (artifactListToVersionList xs) ++ [artifactToVersion x]

artifactListToString :: ArtifactList -> [String]
artifactListToString [] = []
artifactListToString (x:xs) = (artifactListToString xs) ++ [toString x]

artifactToDocument :: Artifact -> DocumentOrDirectory
artifactToDocument (Artifact (Left (Branch _ _ document))) = document
artifactToDocument (Artifact (Right (Snapshot _ _ document))) = document

getArtifactDocument = artifactToDocument
getArtifactContents = artifactToDocument

getArtifactTimestamp :: Artifact -> Timestamp
getArtifactTimestamp (Artifact (Left (Branch _ _ _))) = 0
getArtifactTimestamp (Artifact (Right (Snapshot timestamp _ _))) = timestamp

getArtifactName :: Artifact -> String
getArtifactName (Artifact (Left (Branch name _ _) ) ) = name
getArtifactName (Artifact (Right (Snapshot _ _ _) ) ) = ""

artifactHasVersion :: Artifact -> Version -> Bool
artifactHasVersion (Artifact (Left (Branch _ v1 _))) v2 = (v1 == v2)
artifactHasVersion (Artifact (Right (Snapshot _ v1 _))) v2 = (v1 == v2)

editArtifact :: DocumentOrDirectory -> Artifact -> Artifact 
editArtifact newDD (Artifact (Left (Branch branchName version oldDD))) = liftBranch $ Branch branchName version newDD
editArtifact _     (Artifact (Right snapshot@(Snapshot _ _ _))) = liftSnapshot $ snapshot

isSnapshot :: Artifact -> Bool
isSnapshot (Artifact (Right (Snapshot _ _ _))) = True
isSnapshot _ = False

isBranch :: Artifact -> Bool
isBranch (Artifact (Left (Branch _ _ _))) = True
isBranch _ = False

instance DimensionOperations Artifact where 
    appendDimension (Artifact (Left (Branch branchName v document ))) = Artifact (Left (Branch branchName (appendDimension v) document ))
    appendDimension (Artifact (Right (Snapshot timestamp v document ))) = Artifact (Right (Snapshot timestamp (appendDimension v) document ))
    getNumberOfDimensions (Artifact (Left (Branch _ v _))) = getNumberOfDimensions v
    getNumberOfDimensions (Artifact (Right (Snapshot _ v _))) = getNumberOfDimensions v
    getActualNumberOfDimensions (Artifact (Left (Branch _ v _))) = getActualNumberOfDimensions v
    getActualNumberOfDimensions (Artifact (Right (Snapshot _ v _))) = getActualNumberOfDimensions v
       
data AllowedChanges = Any
                    | None
                    deriving (Show, Eq)

class DetectAllowedChanges entity where
    detectAllowedChanges :: entity -> AllowedChanges

instance DetectAllowedChanges VersionCompound where
    detectAllowedChanges Nothing  = Any
    detectAllowedChanges (Just n) = None 

instance DetectAllowedChanges VersionNumber where
    detectAllowedChanges (VersionNumber vn) = detectAllowedChanges $ Prelude.last vn 
    
instance DetectAllowedChanges Version where
    detectAllowedChanges (Version num) = detectAllowedChanges num
    detectAllowedChanges (MaturityVersion level num) = detectAllowedChanges num

instance DetectAllowedChanges Artifact where
    detectAllowedChanges (Artifact (Left (Branch branchName version _ ))) = detectAllowedChanges version
    detectAllowedChanges (Artifact (Right (Snapshot timestamp version _ ))) = detectAllowedChanges version

allowedChangesToString :: AllowedChanges -> String
allowedChangesToString None = "None"
allowedChangesToString Any = "Any"

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
snapshot1 = Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( liftDocument doc1 )

snapshot2 :: Snapshot2
snapshot2 = Snapshot2 12372  ( liftDocument doc1 )

snapshot3 :: Snapshot
snapshot3 = Snapshot 12372 ( Version $ VersionNumber [Just 11] ) ( liftDocument $ Document "document1" "content11" )

branch1 :: Branch
branch1 = Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( liftDocument doc2 )

branch2 :: Branch
branch2 = Branch "branch22" ( Version $ VersionNumber [Nothing] ) ( liftDocument $ Document "document22" "content_branch22" )

artifact1 :: Artifact
artifact1 = liftSnapshot $ snapshot1

artifact2 :: Artifact
artifact2 = liftBranch $ branch1

artifact3 :: Artifact
artifact3 = liftBranch $ branch2

artifact4 :: Artifact
artifact4 = liftSnapshot $ snapshot3

