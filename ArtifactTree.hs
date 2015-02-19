{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, NoMonomorphismRestriction #-}
module ArtifactTree where

import Data.Tree
import Data.Tree.Pretty
import Data.Time
import Data.Time.Clock.POSIX
import Control.Applicative
import Control.Arrow

import Document
import Artifact
import RoseTree
import Version
import VersionNumber
import VersionTree

import qualified Data.Aeson as JSON
import qualified Data.Text as T

type RoseTreeArtifact = RoseTree Artifact
data ArtifactTree = ArtifactTree RoseTreeArtifact NumberOfDimensions deriving (Show)
type RoseTreeArtifactList = [RoseTreeArtifact]
type ArtifactTreeList = [ArtifactTree]

liftRoseTreeArtifact :: RoseTreeArtifact -> NumberOfDimensions -> ArtifactTree
liftRoseTreeArtifact x dim = ArtifactTree x dim 

rTreeToATree :: RoseTreeArtifactList -> NumberOfDimensions -> ArtifactTreeList
rTreeToATree [] _ = [ ]
rTreeToATree (x:xs) dim = [(ArtifactTree x dim)] ++ (rTreeToATree xs dim)

-- ARTIFACT TREE OPERATIONS --

-- data ArtifactTreeOperation  = First
                            -- | Last
                            -- | Previous
                            -- | Next
                            -- | Parent -- do we really need it here?
                            -- | Children -- do we really need it here?

-- INITIAL ARTIFACT DEFINITIONS --

initialArtifactTree :: RoseTreeArtifact 
initialArtifactTree = RoseTree ( liftSnapshot $ (Snapshot 0 (Version $ VersionCompound (Number 0) ) ) ( liftDocument $ Document "" "") ) 
 [ RoseTree ( liftBranch $ (Branch "trunk" ( Version $ VersionCompound $ NumberPlaceholder ) ) (liftDocument $ Document "" "") ) [] ]
    
{-instance IsInitialArtifact ArtifactTree where
    isInitialArtifact (ArtifactTree aTree _ )= case aTree of 
        ( RoseTree (Artifact ( Right ( Snapshot _ v1 _ ) ) ) [ RoseTree ( Artifact ( Left ( Branch _ v2 _ ) ) ) [] ] ) -> isInitialVersion v1 && isInitialVersion v2
        _ -> False
-}
-- ARTIFACT TREE CONVERSION TO STRING --

class ArtifactTreeToStringTree a where 
    artifactTreeToStringTree :: a -> StringTree
    artifactListToStringTreeList :: [a] -> [StringTree]
    artifactTreeToAllowedChangesTree :: [a] -> [StringTree]

instance ArtifactTreeToStringTree ArtifactTree where
    artifactTreeToStringTree ( ArtifactTree (RoseTree artifact []) _ ) = Node (artifactToString artifact) []
    artifactTreeToStringTree ( ArtifactTree (RoseTree artifact (x:[]) ) _ ) = Node (artifactToString artifact) [ artifactTreeToStringTree x ]
    artifactTreeToStringTree ( ArtifactTree (RoseTree artifact (x:xs) ) _ ) = Node (artifactToString artifact) ( (artifactTreeToStringTree x) : (artifactListToStringTreeList xs) )
    artifactListToStringTreeList [] = []
    artifactListToStringTreeList (x:[]) = [artifactTreeToStringTree x]
    artifactListToStringTreeList (x:xs) = (artifactTreeToStringTree x):(artifactListToStringTreeList xs) 
--    artifactTreeToAllowedChangesTree ( ArtifactTree (RoseTree artifact []) _ ) = Node (allowedChangesToString ( detectAllowedChanges artifact)) []
--    artifactTreeToAllowedChangesTree ( ArtifactTree (RoseTree artifact (x:[])) _ ) = Node (allowedChangesToString ( detectAllowedChanges artifact)) [ artifactTreeToAllowedChangesTree x ]
--    artifactTreeToAllowedChangesTree ( ArtifactTree (RoseTree artifact (x:xs)) _ ) = Node (allowedChangesToString ( detectAllowedChanges artifact)) ( (artifactTreeToAllowedChangesTree x) : (artifactTreeToAllowedChangesTree xs) )

instance ArtifactTreeToStringTree RoseTreeArtifact where
    artifactTreeToStringTree (RoseTree artifact []) = Node (artifactToString artifact) []
    artifactTreeToStringTree (RoseTree artifact (x:[]) ) = Node (artifactToString artifact) [ artifactTreeToStringTree x ]
    artifactTreeToStringTree (RoseTree artifact (x:xs) ) = Node (artifactToString artifact) ( (artifactTreeToStringTree x) : (artifactListToStringTreeList xs) )
    artifactListToStringTreeList [] = []
    artifactListToStringTreeList (x:[]) = [artifactTreeToStringTree x]
    artifactListToStringTreeList (x:xs) = (artifactTreeToStringTree x):(artifactListToStringTreeList xs)
--    artifactTreeToAllowedChangesTree [] = []
--    artifactTreeToAllowedChangesTree (x:[]) = [artifactTreeToAllowedChangesTree x]
--    artifactTreeToAllowedChangesTree (x:xs) = (artifactTreeToAllowedChangesTree x):(artifactTreeToAllowedChangesTree xs) 
    
-- ARTIFACT TREE CONVERSION TO VERSION TREE --

artifactTreeListToVersionTreeList :: RoseTreeArtifactList -> VersionTreeList
artifactTreeListToVersionTreeList [] = []
artifactTreeListToVersionTreeList (x:[]) = [artifactTreeToVersionTree x]
artifactTreeListToVersionTreeList (x:xs) = (artifactTreeToVersionTree x):(artifactTreeListToVersionTreeList xs) 

artifactTreeToVersionTree :: RoseTreeArtifact -> VersionTree
artifactTreeToVersionTree (RoseTree artifact []) = RoseTree (artifactToVersion artifact) []
artifactTreeToVersionTree (RoseTree artifact (x:[])) = RoseTree (artifactToVersion artifact) [ artifactTreeToVersionTree x ]
artifactTreeToVersionTree (RoseTree artifact (x:xs)) = RoseTree (artifactToVersion artifact) ( (artifactTreeToVersionTree x) : (artifactTreeListToVersionTreeList xs) )

-- ARTIFACT TREE SEARCH OPERATIONS --
{-
class SearchArtifactTreeList searchEntity where
    searchArtifactTreeList :: ArtifactTreeList -> searchEntity -> [Artifact] 

instance SearchArtifactTreeList Version where
    searchArtifactTreeList [] _ = []
    searchArtifactTreeList (artifactTree:[]) version = searchArtifactTree artifactTree version
    searchArtifactTreeList (x:xs) version = (searchArtifactTree x version) ++ (searchArtifactTreeList xs version)

instance SearchArtifactTreeList Artifact where
    searchArtifactTreeList [] _ = []
    searchArtifactTreeList (artifactTree:[]) artifact = searchArtifactTree artifactTree artifact
    searchArtifactTreeList (x:xs) artifact = (searchArtifactTree x artifact) ++ (searchArtifactTreeList xs artifact)

instance SearchArtifactTreeList Timestamp where
    searchArtifactTreeList [] _ = []
    searchArtifactTreeList (artifactTree:[]) timestamp = searchArtifactTree artifactTree timestamp
    searchArtifactTreeList (x:xs) timestamp = (searchArtifactTree x timestamp) ++ (searchArtifactTreeList xs timestamp)

instance SearchArtifactTreeList BranchName where
    searchArtifactTreeList [] _ = []
    searchArtifactTreeList (artifactTree:[]) name = searchArtifactTree artifactTree name
    searchArtifactTreeList (x:xs) name = (searchArtifactTree x name) ++ (searchArtifactTreeList xs name)

class SearchArtifactTree searchEntity where
    searchArtifactTree :: ArtifactTree -> searchEntity -> [Artifact] 

instance SearchArtifactTree Version where
    searchArtifactTree ( ArtifactTree (RoseTree artifact [] ) _ ) version = case (artifactHasVersion artifact version) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( ArtifactTree (RoseTree artifact (x:xs) ) dim ) version = (searchArtifactTree (ArtifactTree (RoseTree artifact []) dim ) version ) ++ (searchArtifactTree ( ArtifactTree x dim ) version) ++ (searchArtifactTreeList ( rTreeToATree xs dim ) version)

instance SearchArtifactTree Artifact where
    searchArtifactTree ( ArtifactTree (RoseTree artifact1 [] ) _ ) artifact2 = case (artifact1 == artifact2) of 
        True -> [artifact1]
        False -> []
    searchArtifactTree ( ArtifactTree (RoseTree artifact1 (x:xs) ) dim ) artifact2 = (searchArtifactTree (ArtifactTree (RoseTree artifact1 []) dim ) artifact2) ++ (searchArtifactTree ( ArtifactTree x dim ) artifact2) ++ (searchArtifactTreeList ( rTreeToATree xs dim ) artifact2)

instance SearchArtifactTree Timestamp where
    searchArtifactTree ( ArtifactTree (RoseTree artifact [] ) _ ) timestamp = case (getArtifactTimestamp artifact == timestamp) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( ArtifactTree ( RoseTree artifact (x:xs) ) dim ) timestamp = (searchArtifactTree (ArtifactTree (RoseTree artifact []) dim ) timestamp) ++ (searchArtifactTree ( ArtifactTree x dim ) timestamp) ++ (searchArtifactTreeList ( rTreeToATree xs dim ) timestamp)

instance SearchArtifactTree BranchName where
    searchArtifactTree ( ArtifactTree (RoseTree artifact [] ) _ ) name = case (getArtifactName artifact == name) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( ArtifactTree ( RoseTree artifact (x:xs) ) dim ) name = (searchArtifactTree (ArtifactTree (RoseTree artifact []) dim ) name) ++ (searchArtifactTree ( ArtifactTree x dim ) name) ++ (searchArtifactTreeList ( rTreeToATree xs dim ) name)-}

class SearchRoseTreeArtifactList searchEntity where
    searchRoseTreeArtifactList :: RoseTreeArtifactList -> searchEntity -> [Artifact] 

instance SearchRoseTreeArtifactList Version where
    searchRoseTreeArtifactList [] _ = []
    searchRoseTreeArtifactList (artifactTree:[]) version = searchRoseTreeArtifact artifactTree version
    searchRoseTreeArtifactList (x:xs) version = (searchRoseTreeArtifact x version) ++ (searchRoseTreeArtifactList xs version)
instance SearchRoseTreeArtifactList Artifact where
    searchRoseTreeArtifactList [] _ = []
    searchRoseTreeArtifactList (artifactTree:[]) artifact = searchRoseTreeArtifact artifactTree artifact
    searchRoseTreeArtifactList (x:xs) artifact = (searchRoseTreeArtifact x artifact) ++ (searchRoseTreeArtifactList xs artifact)
instance SearchRoseTreeArtifactList Timestamp where
    searchRoseTreeArtifactList [] _ = []
    searchRoseTreeArtifactList (artifactTree:[]) timestamp = searchRoseTreeArtifact artifactTree timestamp
    searchRoseTreeArtifactList (x:xs) artifact = (searchRoseTreeArtifact x artifact) ++ (searchRoseTreeArtifactList xs artifact)
instance SearchRoseTreeArtifactList BranchName where
    searchRoseTreeArtifactList [] _ = []
    searchRoseTreeArtifactList (artifactTree:[]) branchName = searchRoseTreeArtifact artifactTree branchName
    searchRoseTreeArtifactList (x:xs) artifact = (searchRoseTreeArtifact x artifact) ++ (searchRoseTreeArtifactList xs artifact)


class SearchRoseTreeArtifact searchEntity where
    searchRoseTreeArtifact :: RoseTreeArtifact -> searchEntity -> [Artifact] 

instance SearchRoseTreeArtifact Version where
    searchRoseTreeArtifact (RoseTree artifact [] ) version = case (artifactHasVersion artifact version) of 
        True -> [artifact]
        False -> []
    searchRoseTreeArtifact ( RoseTree artifact (x:xs) ) version = (searchRoseTreeArtifact (RoseTree artifact []) version ) ++ (searchRoseTreeArtifact x version) ++ (searchRoseTreeArtifactList xs version)

instance SearchRoseTreeArtifact Artifact where
    searchRoseTreeArtifact ( RoseTree artifact1 [] ) artifact2 = case (artifact1 == artifact2) of 
        True -> [artifact1]
        False -> []
    searchRoseTreeArtifact ( RoseTree artifact1 (x:xs) ) artifact2 = (searchRoseTreeArtifact (RoseTree artifact1 []) artifact2) ++ (searchRoseTreeArtifact x artifact2) ++ (searchRoseTreeArtifactList xs artifact2)

instance SearchRoseTreeArtifact Timestamp where
    searchRoseTreeArtifact ( RoseTree artifact [] ) timestamp = case (getArtifactTimestamp artifact == timestamp) of 
        True -> [artifact]
        False -> []
    searchRoseTreeArtifact ( RoseTree artifact (x:xs) ) timestamp = (searchRoseTreeArtifact (RoseTree artifact []) timestamp) ++ (searchRoseTreeArtifact x timestamp) ++ (searchRoseTreeArtifactList xs timestamp)

instance SearchRoseTreeArtifact BranchName where
    searchRoseTreeArtifact ( RoseTree artifact [] ) name = case (getArtifactName artifact == name) of 
        True -> [artifact]
        False -> []
    searchRoseTreeArtifact ( RoseTree artifact (x:xs) ) name = (searchRoseTreeArtifact (RoseTree artifact []) name) ++ (searchRoseTreeArtifact x name) ++ (searchRoseTreeArtifactList xs name)

-- FIRST DEPTH SEARCH --
{-
class SearchArtifactTreeChildren a where
    searchArtifactTreeChildren :: ArtifactTreeList -> a -> Bool    

instance SearchArtifactTreeChildren Artifact where
    searchArtifactTreeChildren [] _ = False
    searchArtifactTreeChildren ( ( ArtifactTree ( RoseTree artifact1 _) _ ):xs) artifact2 = (artifact1 == artifact2) || (searchArtifactTreeChildren xs artifact2)
    
instance SearchArtifactTreeChildren Version where
    searchArtifactTreeChildren [] _ = False
    searchArtifactTreeChildren ( ( ArtifactTree ( RoseTree artifact _ ) _ ):xs) version = (artifactHasVersion artifact version) || (searchArtifactTreeChildren xs version)
-}
class SearchRoseTreeArtifactChildren a where
    searchRoseTreeArtifactChildren :: RoseTreeArtifactList -> a -> Bool    

instance SearchRoseTreeArtifactChildren Artifact where
    searchRoseTreeArtifactChildren [] _ = False
    searchRoseTreeArtifactChildren ( ( RoseTree artifact1 _):xs) artifact2 = (artifact1 == artifact2) || (searchRoseTreeArtifactChildren xs artifact2)
    
instance SearchRoseTreeArtifactChildren Version where
    searchRoseTreeArtifactChildren [] _ = False
    searchRoseTreeArtifactChildren ( ( RoseTree artifact _ ):xs) version = (artifactHasVersion artifact version) || (searchRoseTreeArtifactChildren xs version)


-- ARTIFACT TREE LATEST ARTIFACT OPERATIONS --

currentTimeStamp = (round `fmap` getPOSIXTime)

class FindTimestampOfLatestSnapshot searchStructure where
    findTimestampOfLatestSnapshot :: searchStructure -> Timestamp
    
instance FindTimestampOfLatestSnapshot RoseTreeArtifact where    
    findTimestampOfLatestSnapshot ( RoseTree artifact [] ) = getArtifactTimestamp artifact
    findTimestampOfLatestSnapshot ( RoseTree artifact1 list ) = case ( (getArtifactTimestamp artifact1) > (findTimestampOfLatestSnapshot list) ) of
        True -> getArtifactTimestamp artifact1
        False -> findTimestampOfLatestSnapshot list

instance FindTimestampOfLatestSnapshot RoseTreeArtifactList where
    findTimestampOfLatestSnapshot [] = 0
    findTimestampOfLatestSnapshot (x:[]) = findTimestampOfLatestSnapshot x
    findTimestampOfLatestSnapshot (x:xs) = case ( (findTimestampOfLatestSnapshot x) > (findTimestampOfLatestSnapshot xs) ) of
        True -> (findTimestampOfLatestSnapshot x)
        False -> (findTimestampOfLatestSnapshot xs)

class FindVersionOfLatestSnapshot searchStructure where
    findVersionOfLatestSnapshot :: searchStructure -> Version
instance FindVersionOfLatestSnapshot RoseTreeArtifactList where
     findVersionOfLatestSnapshot list = case (searchRoseTreeArtifactList list (findTimestampOfLatestSnapshot list)) of
         [] -> initialVersion ( NumberPlaceholder )
         (x:xs) -> artifactToVersion x
instance FindVersionOfLatestSnapshot RoseTreeArtifact where
     findVersionOfLatestSnapshot rTree = case (searchRoseTreeArtifact rTree ( findTimestampOfLatestSnapshot rTree ) ) of
         [] -> initialVersion (NumberPlaceholder) 
         (x:xs) -> artifactToVersion x
         
class GetArtifactOfLatestSnapshot searchStructure where
     getArtifactOfLatestSnapshot :: searchStructure -> Artifact
    
instance GetArtifactOfLatestSnapshot RoseTreeArtifactList where
     getArtifactOfLatestSnapshot list = case (searchRoseTreeArtifactList list (findTimestampOfLatestSnapshot list)) of
         [] -> initialArtifact(NumberPlaceholder)
         (x:xs) -> x

instance GetArtifactOfLatestSnapshot RoseTreeArtifact where
     getArtifactOfLatestSnapshot aTree = case (searchRoseTreeArtifact aTree ( findTimestampOfLatestSnapshot aTree ) ) of
         [] -> initialArtifact(NumberPlaceholder)
         (x:xs) -> x

class FindVersionOfLatestExperimentalSnapshot a where
        findVersionOfLatestExperimentalSnapshot :: a -> Version

instance FindVersionOfLatestExperimentalSnapshot RoseTreeArtifact where
        findVersionOfLatestExperimentalSnapshot (RoseTree artifact list) = case (isExperimentalSnapshot (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestExperimentalSnapshot list )
                False -> findVersionOfLatestExperimentalSnapshot list  

instance FindVersionOfLatestExperimentalSnapshot RoseTreeArtifactList where
        findVersionOfLatestExperimentalSnapshot [] = initialVersion (NumberPlaceholder)
        findVersionOfLatestExperimentalSnapshot (x:xs) = max ( findVersionOfLatestExperimentalSnapshot x ) (findVersionOfLatestExperimentalSnapshot xs) 

class FindVersionOfLatestReleaseSnapshot a where
        findVersionOfLatestReleaseSnapshot :: a -> Version

instance FindVersionOfLatestReleaseSnapshot RoseTreeArtifact where
        findVersionOfLatestReleaseSnapshot (RoseTree artifact list) = case (isReleaseSnapshot (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestReleaseSnapshot list )
                False -> findVersionOfLatestReleaseSnapshot list  

instance FindVersionOfLatestReleaseSnapshot RoseTreeArtifactList where
        findVersionOfLatestReleaseSnapshot [] = initialVersion (NumberPlaceholder)
        findVersionOfLatestReleaseSnapshot (x:xs) = max ( findVersionOfLatestReleaseSnapshot x ) (findVersionOfLatestReleaseSnapshot xs) 

class FindVersionOfLatestSupportSnapshot a where
        findVersionOfLatestSupportSnapshot :: a -> Version

instance FindVersionOfLatestSupportSnapshot RoseTreeArtifact where
        findVersionOfLatestSupportSnapshot (RoseTree artifact list) = case (isReleaseSnapshot (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestSupportSnapshot list )
                False -> findVersionOfLatestSupportSnapshot list  

instance FindVersionOfLatestSupportSnapshot RoseTreeArtifactList where
        findVersionOfLatestSupportSnapshot [] = initialVersion (NumberPlaceholder)
        findVersionOfLatestSupportSnapshot (x:xs) = max ( findVersionOfLatestSupportSnapshot x ) (findVersionOfLatestSupportSnapshot xs) 

class FindVersionOfLatestReleaseBranch a where
        findVersionOfLatestReleaseBranch :: a -> Version

instance FindVersionOfLatestReleaseBranch RoseTreeArtifact where
        findVersionOfLatestReleaseBranch (RoseTree artifact list) = case (isReleaseBranch (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestReleaseBranch list )
                False -> findVersionOfLatestReleaseBranch list  

instance FindVersionOfLatestReleaseBranch RoseTreeArtifactList where
        findVersionOfLatestReleaseBranch [] = initialVersion (NumberPlaceholder)
        findVersionOfLatestReleaseBranch (x:xs) = max ( findVersionOfLatestReleaseBranch x ) (findVersionOfLatestReleaseBranch xs) 

class FindVersionOfLatestSupportBranch a where
        findVersionOfLatestSupportBranch :: a -> Version

instance FindVersionOfLatestSupportBranch RoseTreeArtifact where
        findVersionOfLatestSupportBranch (RoseTree artifact list) = case (isSupportBranch (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestSupportBranch list )
                False -> findVersionOfLatestSupportBranch list

instance FindVersionOfLatestSupportBranch RoseTreeArtifactList where
        findVersionOfLatestSupportBranch [] = initialVersion (NumberPlaceholder)
        findVersionOfLatestSupportBranch (x:xs) = max ( findVersionOfLatestSupportBranch x ) (findVersionOfLatestSupportBranch xs) 

class FindParentArtifact a where 
    findParentArtifact :: a -> Artifact -> Artifact

instance FindParentArtifact RoseTreeArtifact where
    findParentArtifact (  RoseTree _ [] )  _ = initialArtifact (NumberPlaceholder)
    findParentArtifact (  RoseTree parentArtifact list ) artifact = case (searchRoseTreeArtifactChildren list artifact) of
        True -> parentArtifact
        False -> findParentArtifact list artifact
        
instance FindParentArtifact RoseTreeArtifactList where
    findParentArtifact [] _ = initialArtifact (NumberPlaceholder)
    findParentArtifact (x:xs) artifact = case ( isInitialArtifact (findParentArtifact x artifact) ) of
        True -> findParentArtifact xs artifact
        False -> findParentArtifact x artifact

-- class FindParentArtifacts a where 
    -- findParentArtifacts :: a -> ArtifactList -> ArtifactList

-- instance FindParentArtifacts ArtifactTree where
    -- findParentArtifacts tree [] = []
    -- findParentArtifacts tree (x:xs) = ( [findParentArtifact tree x] ) ++ ( findParentArtifacts tree xs )
    
-- instance FindParentArtifacts ArtifactTreeList where
    -- findParentArtifacts list [] = []
    -- findParentArtifacts list (x:xs) = ( [findParentArtifact list x] ) ++ ( findParentArtifacts list xs )

-- ARTIFACT TREE: GENERATION OF NEW ARTIFACTS --

{-class GenerateSnapshot a where 
    generateSnapshot :: ArtifactTree -> a -> ArtifactTree
-}
-- instance GenerateSnapshot Artifact where
    -- generateSnapshot (ArtifactTree aTree _ ) artifact@(Branch _ _ _) = treeInsert aTree artifact ( Snapshot ( ( getArtifactTimestamp latestArtifact ) + 1 ) (generateNewVersion ( getArtifactVersion latestArtifact ) ) ( artifactToDocument artifact ) ) where latestArtifact = getArtifactOfLatestSnapshot aTree
    -- generateSnapshot (ArtifactTree aTree _ ) artifact@(Snapshot _ _ _) = aTree

x -: f = f x    

class GenerateSnapshot a where 
    generateSnapshot :: a -> RoseTreeArtifact -> RoseTreeArtifact

instance GenerateSnapshot Artifact where
    generateSnapshot artifact@(Artifact (Left (Branch _ _ _ ))) aTree = treeInsert aTree artifact (liftSnapshot $ Snapshot ( (getArtifactTimestamp latestArtifact) + 1 ) (generateNewVersion (getArtifactVersion latestArtifact )) (artifactToDocument artifact)) where latestArtifact = getArtifactOfLatestSnapshot aTree
    generateSnapshot artifact@(Artifact (Right (Snapshot _ _ _ ))) aTree = aTree


generateExperimentalVersionNumberFrom :: BranchName -> RoseTreeArtifact -> Version
generateExperimentalVersionNumberFrom branchName aTree = incrementedVersion
        where 
                branch = searchRoseTreeArtifact aTree branchName !! 0
                branchVersion = getArtifactVersion branch
                latestVersion = case (isExperimentalBranch branchVersion) of 
                        True ->  (findVersionOfLatestExperimentalSnapshot aTree)
                        False -> case (isReleaseBranch branchVersion) of
                                True -> (findVersionOfLatestReleaseSnapshot aTree)
                                False -> case (isSupportBranch branchVersion) of
                                        True -> (findVersionOfLatestSupportSnapshot aTree)
                                        False -> initialVersion (NumberPlaceholder)
                incrementedVersion = case (isInitialVersion latestVersion) of
                        True -> freezeExperimentalVersion  branchVersion
                        False -> case (isExperimentalBranch branchVersion) of
                                True -> increment latestVersion
                                False -> case (isReleaseBranch branchVersion) of 
                                        True -> incrementReleaseNumberForVersion latestVersion
                                        False -> case (isSupportBranch branchVersion) of
                                                True -> incrementSupportNumberForVersion latestVersion
                                                False -> latestVersion

generateReleaseVersionNumberFrom :: BranchName -> RoseTreeArtifact -> Version
generateReleaseVersionNumberFrom branchName aTree = incrementedVersion
        where 
                branch = searchRoseTreeArtifact aTree branchName !! 0
                branchVersion = getArtifactVersion branch
                latestVersion = case (isExperimentalBranch branchVersion) of 
                        True ->  (findVersionOfLatestExperimentalSnapshot aTree)
                        False -> case (isReleaseBranch branchVersion) of
                                True -> (findVersionOfLatestReleaseSnapshot aTree)
                                False -> case (isSupportBranch branchVersion) of
                                        True -> (findVersionOfLatestSupportSnapshot aTree)
                                        False -> initialVersion (NumberPlaceholder)
                incrementedVersion = case (isInitialVersion latestVersion) of
                        True -> freezeReleaseVersion branchVersion
                        False -> case (isExperimentalBranch branchVersion) of
                                True -> incrementReleaseNumberForVersion latestVersion
                                False -> case (isReleaseBranch branchVersion) of 
                                        True -> incrementReleaseNumberForVersion latestVersion
                                        False -> case (isSupportBranch branchVersion) of
                                                True -> incrementReleaseNumberForVersion latestVersion
                                                False -> latestVersion

instance GenerateSnapshot BranchName where
    generateSnapshot branchName aTree = treeInsert aTree toBranchArtifact (liftSnapshot $ Snapshot ( (getArtifactTimestamp latestArtifact) + 1 ) newVersion (artifactToDocument toBranchArtifact)) 
        where 
           toBranchArtifact = searchRoseTreeArtifact aTree branchName !! 0
           toBranchVersion = getArtifactVersion toBranchArtifact
           {-latestArtifact = getArtifactOfLatestSnapshot aTree-}
           latestArtifact = getArtifactOfLatestSnapshot aTree
           parentBranch = findParentArtifact aTree latestArtifact
           {-latestVersion = findVersionOfLatestExperimentalSnapshot aTree-}
           latestVersion = findVersionOfLatestExperimentalSnapshot aTree
           newVersion = case (isReleaseBranch toBranchVersion) of 
                True -> generateNewVersion latestVersion 
                False -> case (isSupportBranch toBranchVersion) of
                        True -> generateNewVersion latestVersion
                        False -> generateNewVersion latestVersion
           {-newVersion = generateExperimentalVersionNumberFrom branchName-}

class GenerateBranch a where
    generateBranchFromSnapshot :: a -> BranchName -> RoseTreeArtifact -> RoseTreeArtifact 
    generateBranchFromBranch :: a -> BranchName -> RoseTreeArtifact -> RoseTreeArtifact 
    generateBranch :: a -> BranchName -> RoseTreeArtifact -> RoseTreeArtifact 

{-instance GenerateBranch Version where-}
    {-generateBranch version branchName aTree = treeInsert aTree snapshot branch-}
        {-where-}
            {-snapshot = searchRoseTreeArtifact aTree version !! 0-}
            {-parentBranch = findParentArtifact aTree snapshot-}
            {-versionOfParentBranch = getArtifactVersion parentBranch-}
            {-branch = liftBranch $ Branch branchName ( versionOfParentBranch ) (artifactToDocument snapshot)-}

instance GenerateBranch String where
    generateBranch versionString branchName aTree = case (isSnapshot artifact) of
        True -> generateBranchFromSnapshot versionString branchName aTree
        False -> generateBranchFromBranch fromBranch branchName aTree
        where 
            fromBranch = getArtifactName artifact
            version = stringToVersion versionString
            artifact = searchRoseTreeArtifact aTree version !! 0
            
    generateBranchFromSnapshot versionString branchName aTree = treeInsert aTree artifact branch
        where
            version = stringToVersion versionString
            artifact = searchRoseTreeArtifact aTree version !! 0
            parentBranch = findParentArtifact aTree artifact
            versionOfParentBranch = getArtifactVersion parentBranch
            branch = liftBranch $ Branch branchName ( versionOfParentBranch ) (artifactToDocument artifact)

{-generateBranchFromBranch :: BranchName -> BranchName -> RoseTreeArtifact -> RoseTreeArtifact -}
    generateBranchFromBranch fromBranchName toBranchName aTree = treeInsert treeWithArtifact artifact branch
        where
            treeWithArtifact = generateSnapshot fromBranchName aTree
            version = findVersionOfLatestExperimentalSnapshot treeWithArtifact 
            artifact = searchRoseTreeArtifact treeWithArtifact version !! 0
            parentBranch = findParentArtifact treeWithArtifact artifact
            versionOfParentBranch = getArtifactVersion parentBranch
            branch = liftBranch $ Branch toBranchName ( versionOfParentBranch ) (artifactToDocument artifact)
                    
createExperimentalBranch = generateBranch

treeDimensionsMoreThanTwo :: RoseTreeArtifact -> Bool
treeDimensionsMoreThanTwo aTree = getNumberOfDimensions ( searchRoseTreeArtifact aTree (Version $ VersionCompound $ NumberPlaceholder) !! 0 ) >= Number 2

treeDimensionsMoreThanThree :: RoseTreeArtifact -> Bool
treeDimensionsMoreThanThree aTree = getNumberOfDimensions ( searchRoseTreeArtifact aTree (Version $ VersionCompound $ NumberPlaceholder) !! 0 ) >= Number 3

createReleaseBranch :: BranchName -> RoseTreeArtifact -> RoseTreeArtifact 
createReleaseBranch branchName aTree = treeInsert treeWithArtifact artifact branch
        where
                treeWithArtifact = case (treeDimensionsMoreThanTwo aTree) of 
                        True -> generateSnapshot branchName aTree
                        False -> generateSnapshot branchName (appendDimension aTree)
                version = findVersionOfLatestExperimentalSnapshot treeWithArtifact 
                artifact = searchRoseTreeArtifact treeWithArtifact version !! 0
                parentBranch = findParentArtifact treeWithArtifact artifact
                latestReleaseVersion = findVersionOfLatestReleaseBranch treeWithArtifact
                releaseVersion = incrementReleaseNumberForVersion latestReleaseVersion 
                branch = liftBranch $ Branch (versionToString releaseVersion) ( releaseVersion ) (artifactToDocument artifact)

createSupportBranch:: BranchName -> RoseTreeArtifact -> RoseTreeArtifact 
createSupportBranch branchName aTree = treeInsert treeWithArtifact artifact branch
        where
                treeWithArtifact = case (treeDimensionsMoreThanThree aTree) of 
                        True -> generateSnapshot branchName aTree
                        False -> case(treeDimensionsMoreThanTwo aTree) of 
                                True -> generateSnapshot branchName (appendDimension aTree)
                                False -> generateSnapshot branchName (appendDimension (appendDimension aTree))
                version = findVersionOfLatestExperimentalSnapshot treeWithArtifact 
                artifact = searchRoseTreeArtifact treeWithArtifact version !! 0
                parentBranch = findParentArtifact treeWithArtifact artifact
                latestSupportVersion = findVersionOfLatestSupportBranch treeWithArtifact
                supportVersion = incrementSupportNumberForVersion latestSupportVersion 
                branch = liftBranch $ Branch (versionToString supportVersion) ( supportVersion ) (artifactToDocument artifact)


class EditRoseTreeArtifact a where
        editRoseTreeArtifact :: BranchName -> DocumentOrDirectory -> a -> a

instance EditRoseTreeArtifact RoseTreeArtifact where 
        editRoseTreeArtifact name newDD aTree@( RoseTree artifact [] ) = case (getArtifactName artifact == name) of 
                True -> (RoseTree (editArtifact newDD artifact) [] )
                False -> aTree
        editRoseTreeArtifact name newDD ( RoseTree artifact (x:xs) ) = case (getArtifactName artifact == name) of
                True -> (RoseTree (editArtifact newDD artifact) (editRoseTreeArtifact name newDD (x:xs)))
                False -> (RoseTree artifact (editRoseTreeArtifact name newDD (x:xs)))

instance EditRoseTreeArtifact RoseTreeArtifactList where 
    editRoseTreeArtifact _ _ []= []
    editRoseTreeArtifact branchName newDD (x:xs) = (editRoseTreeArtifact branchName newDD x ) : (editRoseTreeArtifact branchName newDD xs )

instance VersionOperations RoseTreeArtifact where 
    appendDimension (RoseTree artifact list ) = RoseTree (appendDimension artifact) (appendDimension list)
    getNumberOfDimensions aTree = getNumberOfDimensions (getArtifactVersion (searchRoseTreeArtifact aTree (Version $ VersionCompound $ NumberPlaceholder) !! 0))
    
instance VersionOperations RoseTreeArtifactList where 
    appendDimension [] = []
    appendDimension (x:xs) = ( appendDimension x ) : (appendDimension xs)

{-editArtifact :: BranchName -> Document -> RoseTreeArtifact -> RoseTreeArtifact -}
{-editArtifact branchName document aTree = searchRoseTreeArtifact aTree branchName -}

-- ARTIFACT TREE: APPLICATION OF DOCUMENT OPERATIONS -- 

-- updateArtifactTree :: ArtifactTree -> ArtifactTreeOperation -> ArtifactTree
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeEdit artifact@(Snapshot _ _ _) documentContent ) = aTree error "Snapshots are not editable"
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeEdit artifact@(Branch branchName version contents ) documentContent ) = treeUpdate aTree artifact ( Branch branchName version documentContent )
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateSnapshot artifact@(Snapshot _ _ _) ) = aTree
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateSnapshot artifact@(Branch _ _ _) ) = generateSnapshot aTree artifact
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateBranch artifact@(Snapshot _ _ document) branchName ) = treeInsert aTree artifact ( Branch branchName (generateNewVersion ( getArtifactVersion ( findParentArtifact aTree artifact ) ) ) document ) 
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateBranch artifact@(Branch _ _ document ) branchName ) = treeInsert newTree ( getArtifactOfLatestSnapshot newTree ) ( Branch branchName (generateNewVersion ( getArtifactVersion artifact ) ) document ) where newTree = (generateSnapshot aTree artifact) 

-- EXAMPLES

artifactTree1 :: RoseTreeArtifact
artifactTree1 = RoseTree ( liftSnapshot $ (Snapshot 1398980989 (Version ( VersionCompound (Number 0) ) ) ( liftDocument $ Document "" "") ) ) []

sArtifactTree1 :: StringTree
sArtifactTree1 = Node "0" []

artifactTree2 :: RoseTreeArtifact
artifactTree2 = RoseTree ( liftSnapshot $ ( Snapshot 1398980989 (Version ( VersionCompound (Number 0) ) ) ( liftDocument $ Document "" "") ) ) 
    [ RoseTree (liftBranch $ (Branch "trunk" (Version (VersionCompound NumberPlaceholder) ) ( liftDocument $ Document "document1" "") ) ) [] ]

sArtifactTree2 :: StringTree
sArtifactTree2 = Node "0" [ Node "trunk (x)" [] ]
    
artifactTree3 :: RoseTreeArtifact
artifactTree3 = RoseTree (liftSnapshot $ Snapshot 1398980989 (Version $ VersionCompound $ Number 0 ) (liftDocument $ Document "" "") ) 
    [ RoseTree (liftBranch $ Branch "trunk" (Version $ VersionCompound $ NumberPlaceholder ) ( liftDocument $ Document "document1" "latest_content") ) [ 
        RoseTree (liftSnapshot $ Snapshot 1398980990 (Version $ VersionCompound $ Number 1) ( liftDocument $ Document "document1" "content1") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (Version $ VersionCompound $ Number 2) ( liftDocument $ Document "document1" "content2") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980992 (Version $ VersionCompound $ Number 3) ( liftDocument $ Document "document1" "content3") ) [
            RoseTree (liftBranch $ Branch "branch1" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch1") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980993 (Version $ VersionCompound $ Number 4) ( liftDocument $ Document "document1" "content4") ) [], 
                RoseTree (liftSnapshot $ Snapshot 1398980995 (Version $ VersionCompound $ Number 6) ( liftDocument $ Document "document1" "content6") ) [] 
            ],
            RoseTree (liftBranch $ Branch "branch2" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch2") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980999 (Version $ VersionCompound $ Number 10) ( liftDocument $ Document "document1" "content10") ) [] 
            ]
        ],
        RoseTree (liftSnapshot $ Snapshot 1398980994 (Version $ VersionCompound $ Number 5) ( liftDocument $ Document "document1" "content5") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980996 (Version $ VersionCompound $ Number 7) ( liftDocument $ Document "document1" "content7") ) [
            RoseTree (liftBranch $ Branch "branch3" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch3") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980998 (Version $ VersionCompound $ Number 9) ( liftDocument $ Document "document1" "content9") ) [] 
            ]
        ], 
        RoseTree (liftSnapshot $ Snapshot 1398980997 (Version $ VersionCompound $ Number 8) ( liftDocument $ Document "document1" "content8") ) []
    ] ]

t = artifactTree3

a1 = liftSnapshot $ Snapshot 1398980990 (Version $ VersionCompound $ Number 1) ( liftDocument $ Document "document1" "content1") 
a5 = liftSnapshot $ Snapshot 1398980994 (Version $ VersionCompound $ Number 5) ( liftDocument $ Document "document1" "content5") 
a6 = liftSnapshot $ Snapshot 1398980995 (Version $ VersionCompound $ Number 6) ( liftDocument $ Document "document1" "content6")
br1 = liftBranch $ Branch "branch1" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch1") 
list = [ RoseTree a1 [], 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (Version $ VersionCompound $ Number 2) ( liftDocument $ Document "document1" "content2") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980992 (Version $ VersionCompound $ Number 3) ( liftDocument $ Document "document1" "content3") ) [] ]
subtree = RoseTree (liftBranch $ Branch "trunk" (Version $ VersionCompound $ NumberPlaceholder ) ( liftDocument $ Document "document1" "latest_content") ) [ 
        RoseTree (liftSnapshot $ Snapshot 1398980990 (Version $ VersionCompound $ Number 1) ( liftDocument $ Document "document1" "content1") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (Version $ VersionCompound $ Number 2) ( liftDocument $ Document "document1" "content2") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980992 (Version $ VersionCompound $ Number 3) ( liftDocument $ Document "document1" "content3") ) [
            RoseTree (liftBranch $ Branch "branch1" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch1") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980993 (Version $ VersionCompound $ Number 4) ( liftDocument $ Document "document1" "content4") ) [], 
                RoseTree (liftSnapshot $ Snapshot 1398980995 (Version $ VersionCompound $ Number 6) ( liftDocument $ Document "document1" "content6") ) [] 
            ]
        ]
    ]

artifactTree4 :: RoseTreeArtifact 
artifactTree4 = RoseTree (liftBranch $ Branch "trunk" (stringToVersion "x.x.x") (liftDocument $ Document "" "")) [
        RoseTree (liftSnapshot $ Snapshot 1398980990 (stringToVersion "x.x.0") (liftDocument $ Document "" "")) [
                RoseTree (liftBranch $ Branch "1.x" (stringToVersion "x.1.x") (liftDocument $ Document "doc1" "release1")) []
        ] , 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (stringToVersion "x.x.1") (liftDocument $ Document "" "")) [
                RoseTree (liftBranch $ Branch "2.x" (stringToVersion "x.2.x") (liftDocument $ Document "doc1" "release2")) [
                        RoseTree (liftSnapshot $ Snapshot 1398980992 (stringToVersion "x.2.0") (liftDocument $ Document "doc1" "release2.0")) []
                ]
        ] , 
        RoseTree (liftSnapshot $ Snapshot 1398980993 (stringToVersion "x.x.2") (liftDocument $ Document "" "")) [
                RoseTree (liftBranch $ Branch "1.x.x" (stringToVersion "1.x.x") (liftDocument $ Document "doc1" "support1")) [
                        RoseTree (liftSnapshot $ Snapshot 1398980994 (stringToVersion "1.x.0") (liftDocument $ Document "doc1" "support1_0")) []
                ]
        ] ]
sArtifactTree3 :: StringTree
sArtifactTree3 = Node "0" [ Node "trunk (x)" 
    [
        Node "1" [],
        Node "2" [],
        Node "3" [
            Node "branch1 (x)" [
                Node "4" [],
                Node "6" []
            ], 
            Node "branch2 (x)" [
                Node "10" []
            ] 
        ],
        Node "5" [],
        Node "7" [
            Node "branch3 (x)" [
                Node "9" []
            ]
        ],
        Node "8" []
    ] ]

-- HELPER FUNCTIONS --

displayArtifactTree t = putStrLn $ drawVerticalTree (artifactTreeToStringTree t)

-- displayRepresentationsOfArtifactTree :: ArtifactTree -> IO ()
-- displayRepresentationsOfArtifactTree (ArtifactTree aTree _ ) = putStrLn $ (drawVerticalTree (artifactTreeToStringTree aTree) ++ "\n" 
--    ++ drawVerticalTree ( versionTreeToStringTree (artifactTreeToVersionTree aTree)) ++ "\n" 
--    ++ drawVerticalTree ( artifactTreeToAllowedChangesTree ( aTree))) ++ "\n"
--    ++ drawVerticalTree ( artifactListToPlatformsTree ( aTree ) (deploy aTree deploymentRules platformDB ) ) ++ "\n"
