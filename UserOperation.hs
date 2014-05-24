{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module UserOperation where

import Artifact
import ArtifactTree
import Version
import VersionTree
import FileOperation

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

instance Action ArtifactTree ArtifactList where
    action First aTree = (searchArtifactTree aTree initialVersion)
    action Last aTree = (searchArtifactTree aTree (findVersionOfLatestSnapshot aTree))

instance Action ArtifactTree ( IO () ) where
    action Save aTree = saveToFile aTree 
    action Describe aTree = displayArtifactTree aTree 
    action Display aTree = displayArtifactTree aTree 
    action Show aTree = displayArtifactTree aTree 
                -- | Output
                -- | List
                -- | Describe
                -- | Explain

instance Action VersionTree ( IO () ) where
    action Save vTree = saveToFile vTree

instance ActionWithInput ArtifactTree BranchName ArtifactTree where
    actionWithInput Append aTree branchName = generateSnapshot aTree $ searchArtifactTree aTree branchName

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