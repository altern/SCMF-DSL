module Artifact where

import Version
import VersionNumber
import MaturityLevel

type BranchName = String
type Timestamp = Integer

type DocumentName = String
type DirectoryName = String
type DocumentContent = String

data DocumentOrDirectory = Document DocumentName DocumentContent 
                         | Directory DirectoryName [DocumentOrDirectory]
                         
instance Show DocumentOrDirectory where
    show (Document name content ) = "Document: " ++ name ++ ", Content: " ++ content ++ ""
    show (Directory dirName content ) = "Directory: " ++ dirName ++ ", Content: " ++ (show content) ++ ""

data Artifact = Branch BranchName Version DocumentOrDirectory
            | Snapshot Timestamp Version DocumentOrDirectory

instance  Eq Artifact where
    (Branch nameA versionA _ ) == (Branch nameB versionB _ )                = (nameA == nameB) && (versionA == versionB)
    (Snapshot timestampA versionA _ ) == (Snapshot timestampB versionB _ )    = (timestampA == timestampB) && (versionA == versionB)
    _ == _                                                                  = False

instance Show Artifact where
    show (Branch branchName version contents ) = ("Branch '" ++ branchName ++ "', Version " ++ (versionToString version) ++ ", " ++ (show contents) ++ "\n")
    show (Snapshot timestamp version contents ) = ("Snapshot taken at " ++ (show timestamp) ++ ", Version " ++ (versionToString version) ++ ", " ++ (show contents) ++ "\n")

artifactToString :: Artifact -> String
artifactToString (Branch branchName version _) = (branchName ++ " (" ++ ( versionToString version ) ++ ")")
artifactToString (Snapshot timestamp version _) = (versionToString version)

artifactToVersion :: Artifact -> Version
artifactToVersion (Branch branchName version _) = version
artifactToVersion (Snapshot timestamp version _) = version

data AllowedChanges = Any
                    | None
                    deriving (Show)

class DetectAllowedChanges entity where
    detectAllowedChanges :: entity -> AllowedChanges

instance DetectAllowedChanges VersionNumber where
    detectAllowedChanges NumberPlaceholder  = Any
    detectAllowedChanges (Number n)         = None 
    
instance DetectAllowedChanges Version where
    detectAllowedChanges (Version num) = detectAllowedChanges num
    detectAllowedChanges (MaturityVersion level num) = detectAllowedChanges num

instance DetectAllowedChanges Artifact where
    detectAllowedChanges (Branch branchName version _ ) = detectAllowedChanges version
    detectAllowedChanges (Snapshot timestamp version _ ) = detectAllowedChanges version

allowedChangesToString :: AllowedChanges -> String
allowedChangesToString None = "None"
allowedChangesToString Any = "Any"

artifactHasVersion :: Artifact -> Version -> Bool
artifactHasVersion (Branch _ v1 _) v2 = (v1 == v2)
artifactHasVersion (Snapshot _ v1 _) v2 = (v1 == v2)

data DocumentOperation = Edit Artifact DocumentContent
                    -- | Save DocumentName Artifact -- Use CreateSnapshot instead
                    -- | Copy DocumentName Artifact -- Use CreateBranch instead
                       | CreateSnapshot Artifact 
                       | CreateBranch Artifact 
                    -- | Share   

data DocumentChangeType = Add
                        | Delete
                        | Update
                        | Rename
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

-- EXAMPLES --

artifact1 :: Artifact
artifact1 = Snapshot 12372 ( Version ( Number 10 ) ) ( Document "document1" "content10" )

artifact2 :: Artifact
artifact2 = Branch "branch3" ( Version NumberPlaceholder ) ( Document "document1" "content_branch3" )

artifact3 :: Artifact
artifact3 = Branch "branch22" ( Version NumberPlaceholder ) ( Document "document22" "content_branch22" )



