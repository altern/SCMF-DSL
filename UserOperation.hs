{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module UserOperation where

import Artifact
import ArtifactTree
import RoseTree
import Version
import VersionTree
import Repository
import Platform
import FileOperation
import Document 

data UserOperation = LoadHistoryFromFile
                   | SaveHistoryToFile
                   | LoadPlatformsFromFile
                   | SavePlatformsToFile
                   | DescribeHistory
                   | DescribeVersion
                   | CreateDeploymentRule
                   | DeleteDeploymentRule
                   | DisplayVersionTree

-- data VersionTreeOperation  = First
                            -- | Last
                            -- | Previous
                            -- | Next
                            -- | Parent -- do we really need it here?
                            -- | Children -- do we really need it here?
                            -- | Merge -- ?

class EmptyAction a where
    emptyAction :: UserAction -> a
    
class Action a b where
    action :: UserAction -> a -> b
    
class ActionWithInput a i b where
    actionWithInput :: UserAction -> a -> i -> b
    
-- data ArtifactTreeOperation  = First
                            -- | Last
                            -- | Previous
                            -- | Next
                            -- | Parent -- do we really need it here?
                            -- | Children -- do we really need it here?
                            
instance EmptyAction ArtifactTree where
    emptyAction Init = initialArtifactTree
    emptyAction Flush = initialArtifactTree
    emptyAction New = initialArtifactTree
    emptyAction Create = initialArtifactTree
    emptyAction Generate = initialArtifactTree
    emptyAction Empty = initialArtifactTree
    
instance EmptyAction VersionTree where
    emptyAction Init = initialVersionTree
    emptyAction Flush = initialVersionTree
    emptyAction New = initialVersionTree
    emptyAction Create = initialVersionTree
    emptyAction Generate = initialVersionTree
    emptyAction Empty = initialVersionTree
    
instance EmptyAction Version where
    emptyAction Init = initialVersion
    emptyAction Flush = initialVersion
    emptyAction New = initialVersion
    emptyAction Create = initialVersion
    emptyAction Generate = initialVersion
    emptyAction Empty = initialVersion
    
instance Action ArtifactTree ArtifactList where
    action First aTree = (searchArtifactTree aTree initialVersion)
    action Last aTree = (searchArtifactTree aTree (findVersionOfLatestSnapshot aTree))

instance Action ArtifactTree ( IO () ) where
    {-action Save aTree = saveToFile aTree -}
    {-action Store aTree = saveToFile aTree -}
    
    action Describe aTree = displayArtifactTree aTree 
    action Display aTree = displayArtifactTree aTree 
    action Show aTree = displayArtifactTree aTree 
    action Output aTree = displayArtifactTree aTree 
    action List aTree = displayArtifactTree aTree 
    action Explain aTree = displayArtifactTree aTree 

instance Action Repository ( IO () ) where
    action Save vTree = saveToFile vTree
    action Store vTree = saveToFile vTree

    action Describe aTree = displayTree aTree 
    action Display aTree = displayTree aTree 
    action Show aTree = displayTree aTree 
    action Output aTree = displayTree aTree 
    action List aTree = displayTree aTree 
    action Explain aTree = displayTree aTree
    
instance Action VersionTree ( IO () ) where
    action Save vTree = saveToFile vTree
    action Store vTree = saveToFile vTree

    action Describe aTree = displayTree aTree 
    action Display aTree = displayTree aTree 
    action Show aTree = displayTree aTree 
    action Output aTree = displayTree aTree 
    action List aTree = displayTree aTree 
    action Explain aTree = displayTree aTree
    
instance Action PlatformDB ( IO () ) where
    action Save db = saveToFile db
    action Store db = saveToFile db

    action Describe db = displayPlatformDB db 
    action Display db = displayPlatformDB db 
    action Show db = displayPlatformDB db 
    action Output db = displayPlatformDB db 
    action List db = displayPlatformDB db 
    action Explain db = displayPlatformDB db  
    
instance Action DeploymentRules ( IO () ) where
    {-action Save rules = saveToFile rules-}
    {-action Store rules = saveToFile rules-}

    action Describe rules = displayDeploymentRules rules 
    action Display rules = displayDeploymentRules rules 
    action Show rules = displayDeploymentRules rules 
    action Output rules = displayDeploymentRules rules 
    action List rules = displayDeploymentRules rules 
    action Explain rules = displayDeploymentRules rules 

instance ActionWithInput ArtifactTree BranchName ArtifactTree where
    actionWithInput Append aTree branchName = generateSnapshot ((searchArtifactTree aTree branchName)!!0) aTree
    actionWithInput Add aTree branchName = generateSnapshot ((searchArtifactTree aTree branchName)!!0) aTree
    actionWithInput Generate aTree branchName = generateSnapshot ((searchArtifactTree aTree branchName)!!0) aTree
    
instance ActionWithInput ArtifactTree (BranchName, DocumentName, DocumentContent) ArtifactTree where
    actionWithInput Edit aTree (branchName, documentName, documentContent) = editArtifactTree branchName (liftDocument $ Document documentName documentContent) aTree
    actionWithInput Update aTree (branchName, documentName, documentContent) = editArtifactTree branchName (liftDocument $ Document documentName documentContent) aTree 
    
instance ActionWithInput ArtifactTree Artifact ArtifactTree where
    actionWithInput Append aTree artifact = generateSnapshot ((searchArtifactTree aTree artifact)!!0) aTree 
    actionWithInput Add aTree artifact = generateSnapshot ((searchArtifactTree aTree artifact)!!0) aTree
    actionWithInput Generate aTree artifact = generateSnapshot ((searchArtifactTree aTree artifact)!!0) aTree
    
instance ActionWithInput VersionTree Version VersionTree where
    actionWithInput Append vTree version = appendNewVersion vTree version
    actionWithInput Add vTree version = appendNewVersion vTree version
    actionWithInput Generate vTree version = appendNewVersion vTree version

data UserAction = Load
                | Restore
                | Save
                | Store
                | New
                | Create
                | Init
                | Add
                | Append
                | Generate
                | Edit
                | Rename
                | Delete
                | Empty
                | Flush
                | Display
                | Show
                | Output
                | List
                | Describe
                | Explain
                | Perform
                | Apply
                | Do
                | Update
                | Find 
                | Get
                | Previous
                | Next
                | First
                | Last
                | Parent 
                | Children
                | Cancel
                
-- EXAMPLE 1 of SCMF DSL PROGRAM: 
-- LoadArtifactTree "artifactTree.json"
-- Edit "trunk" "new file content"
-- Create Snapshot "trunk"
-- Show Latest Version "trunk"
-- Show Latest Artifact "trunk"
-- Save ArtifactTree "artifactTree.json"

-- EXAMPLE 2 of SCMF DSL PROGRAM: 
-- Load PlatformDB "platforms.json"
-- Load DeploymentRules "deploymentRules.json"
-- Load ArtifactTree "artifactTree.json"
-- Apply DeploymentRules 
-- Show PlatformDB
-- Save PlatformDB "platforms.json"
