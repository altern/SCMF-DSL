{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, OverloadedStrings, TypeFamilies, NoMonomorphismRestriction #-}
module ArtifactTree where

import Data.Tree
import Data.Tree.Pretty
import Data.Time
import Data.Time.Clock.POSIX
import Control.Applicative

import Document
import Artifact
import RoseTree
import Version
import VersionNumber
import VersionTree

import qualified Data.Aeson as JSON
import qualified Data.Text as T

type RoseTreeArtifact = RoseTree Artifact
-- data ArtifactTree = ArtifactTree RoseTreeArtifact NumberOfDimensions deriving (Show)
type ArtifactTree = RoseTree Artifact
type RoseTreeArtifactList = [RoseTreeArtifact]
type ArtifactTreeList = [ArtifactTree]

{-liftRoseTreeArtifact :: RoseTreeArtifact -> NumberOfDimensions -> ArtifactTree-}
{-liftRoseTreeArtifact x dim = ArtifactTree x dim -}

{-rTreeToATree :: RoseTreeArtifactList -> NumberOfDimensions -> ArtifactTreeList-}
{-rTreeToATree [] _ = [ ]-}
{-rTreeToATree (x:xs) dim = [(ArtifactTree x dim)] ++ (rTreeToATree xs dim)-}

-- ARTIFACT TREE OPERATIONS --

-- data ArtifactTreeOperation  = First
                            -- | Last
                            -- | Previous
                            -- | Next
                            -- | Parent -- do we really need it here?
                            -- | Children -- do we really need it here?

-- INITIAL ARTIFACT DEFINITIONS --

initialArtifactTree :: ArtifactTree 
initialArtifactTree = RoseTree ( liftSnapshot $ (Snapshot 0 (Version $ VersionNumber [Just 0] ) ) ( liftDocument $ Document "" "") ) 
 [ RoseTree ( liftBranch $ (Branch "trunk" ( Version $ VersionNumber [Nothing] ) ) (liftDocument $ Document "" "") ) [] ]
    
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

{-instance ArtifactTreeToStringTree ArtifactTree where-}
    {-artifactTreeToStringTree ( RoseTree artifact [] ) = Node (show artifact) []-}
    {-artifactTreeToStringTree ( RoseTree artifact (x:[]) ) = Node (show artifact) [ artifactTreeToStringTree x ]-}
    {-artifactTreeToStringTree ( RoseTree artifact (x:xs) ) = Node (show artifact) ( (artifactTreeToStringTree x) : (artifactListToStringTreeList xs) )-}
    {-artifactListToStringTreeList [] = []-}
    {-artifactListToStringTreeList (x:[]) = [artifactTreeToStringTree x]-}
    {-artifactListToStringTreeList (x:xs) = (artifactTreeToStringTree x):(artifactListToStringTreeList xs) -}
--    artifactTreeToAllowedChangesTree ( ArtifactTree (RoseTree artifact []) _ ) = Node (allowedChangesToString ( detectAllowedChanges artifact)) []
--    artifactTreeToAllowedChangesTree ( ArtifactTree (RoseTree artifact (x:[])) _ ) = Node (allowedChangesToString ( detectAllowedChanges artifact)) [ artifactTreeToAllowedChangesTree x ]
--    artifactTreeToAllowedChangesTree ( ArtifactTree (RoseTree artifact (x:xs)) _ ) = Node (allowedChangesToString ( detectAllowedChanges artifact)) ( (artifactTreeToAllowedChangesTree x) : (artifactTreeToAllowedChangesTree xs) )

instance ArtifactTreeToStringTree ArtifactTree where
    artifactTreeToStringTree (RoseTree artifact []) = Node (show artifact) []
    artifactTreeToStringTree (RoseTree artifact (x:[]) ) = Node (show artifact) [ artifactTreeToStringTree x ]
    artifactTreeToStringTree (RoseTree artifact (x:xs) ) = Node (show artifact) ( (artifactTreeToStringTree x) : (artifactListToStringTreeList xs) )
    artifactListToStringTreeList [] = []
    artifactListToStringTreeList (x:[]) = [artifactTreeToStringTree x]
    artifactListToStringTreeList (x:xs) = (artifactTreeToStringTree x):(artifactListToStringTreeList xs)
--    artifactTreeToAllowedChangesTree [] = []
--    artifactTreeToAllowedChangesTree (x:[]) = [artifactTreeToAllowedChangesTree x]
--    artifactTreeToAllowedChangesTree (x:xs) = (artifactTreeToAllowedChangesTree x):(artifactTreeToAllowedChangesTree xs) 
    
-- ARTIFACT TREE CONVERSION TO VERSION TREE --

artifactTreeListToVersionTreeList :: ArtifactTreeList -> VersionTreeList
artifactTreeListToVersionTreeList [] = []
artifactTreeListToVersionTreeList (x:[]) = [artifactTreeToVersionTree x]
artifactTreeListToVersionTreeList (x:xs) = (artifactTreeToVersionTree x):(artifactTreeListToVersionTreeList xs) 

artifactTreeToVersionTree :: ArtifactTree -> VersionTree
artifactTreeToVersionTree (RoseTree artifact []) = RoseTree (artifactToVersion artifact) []
artifactTreeToVersionTree (RoseTree artifact (x:[])) = RoseTree (artifactToVersion artifact) [ artifactTreeToVersionTree x ]
artifactTreeToVersionTree (RoseTree artifact (x:xs)) = RoseTree (artifactToVersion artifact) ( (artifactTreeToVersionTree x) : (artifactTreeListToVersionTreeList xs) )

-- ARTIFACT TREE SEARCH OPERATIONS --
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
    searchArtifactTreeList (x:xs) artifact = (searchArtifactTree x artifact) ++ (searchArtifactTreeList xs artifact)
instance SearchArtifactTreeList BranchName where
    searchArtifactTreeList [] _ = []
    searchArtifactTreeList (artifactTree:[]) branchName = searchArtifactTree artifactTree branchName
    searchArtifactTreeList (x:xs) artifact = (searchArtifactTree x artifact) ++ (searchArtifactTreeList xs artifact)


class SearchArtifactTree searchEntity where
    searchArtifactTree :: ArtifactTree -> searchEntity -> [Artifact] 

instance SearchArtifactTree Version where
    searchArtifactTree (RoseTree artifact [] ) version = case (artifactHasVersion artifact version) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( RoseTree artifact (x:xs) ) version = (searchArtifactTree (RoseTree artifact []) version ) ++ (searchArtifactTree x version) ++ (searchArtifactTreeList xs version)

instance SearchArtifactTree Artifact where
    searchArtifactTree ( RoseTree artifact1 [] ) artifact2 = case (artifact1 == artifact2) of 
        True -> [artifact1]
        False -> []
    searchArtifactTree ( RoseTree artifact1 (x:xs) ) artifact2 = (searchArtifactTree (RoseTree artifact1 []) artifact2) ++ (searchArtifactTree x artifact2) ++ (searchArtifactTreeList xs artifact2)

instance SearchArtifactTree Timestamp where
    searchArtifactTree ( RoseTree artifact [] ) timestamp = case (getArtifactTimestamp artifact == timestamp) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( RoseTree artifact (x:xs) ) timestamp = (searchArtifactTree (RoseTree artifact []) timestamp) ++ (searchArtifactTree x timestamp) ++ (searchArtifactTreeList xs timestamp)

instance SearchArtifactTree BranchName where
    searchArtifactTree ( RoseTree artifact [] ) name = case (getArtifactName artifact == name) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( RoseTree artifact (x:xs) ) name = (searchArtifactTree (RoseTree artifact []) name) ++ (searchArtifactTree x name) ++ (searchArtifactTreeList xs name)

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
class SearchArtifactTreeChildren a where
    searchArtifactTreeChildren :: ArtifactTreeList -> a -> Bool    

instance SearchArtifactTreeChildren Artifact where
    searchArtifactTreeChildren [] _ = False
    searchArtifactTreeChildren ( ( RoseTree artifact1 _):xs) artifact2 = (artifact1 == artifact2) || (searchArtifactTreeChildren xs artifact2)
    
instance SearchArtifactTreeChildren Version where
    searchArtifactTreeChildren [] _ = False
    searchArtifactTreeChildren ( ( RoseTree artifact _ ):xs) version = (artifactHasVersion artifact version) || (searchArtifactTreeChildren xs version)


-- ARTIFACT TREE LATEST ARTIFACT OPERATIONS --

currentTimeStamp = (round `fmap` getPOSIXTime)

class FindTimestampOfLatestSnapshot searchStructure where
    findTimestampOfLatestSnapshot :: searchStructure -> Timestamp
    
instance FindTimestampOfLatestSnapshot ArtifactTree where    
    findTimestampOfLatestSnapshot ( RoseTree artifact [] ) = getArtifactTimestamp artifact
    findTimestampOfLatestSnapshot ( RoseTree artifact1 list ) = case ( (getArtifactTimestamp artifact1) > (findTimestampOfLatestSnapshot list) ) of
        True -> getArtifactTimestamp artifact1
        False -> findTimestampOfLatestSnapshot list

instance FindTimestampOfLatestSnapshot ArtifactTreeList where
    findTimestampOfLatestSnapshot [] = 0
    findTimestampOfLatestSnapshot (x:[]) = findTimestampOfLatestSnapshot x
    findTimestampOfLatestSnapshot (x:xs) = case ( (findTimestampOfLatestSnapshot x) > (findTimestampOfLatestSnapshot xs) ) of
        True -> (findTimestampOfLatestSnapshot x)
        False -> (findTimestampOfLatestSnapshot xs)

class FindVersionOfLatestSnapshot searchStructure where
    findVersionOfLatestSnapshot :: searchStructure -> Version
instance FindVersionOfLatestSnapshot ArtifactTreeList where
     findVersionOfLatestSnapshot list = case (searchArtifactTreeList list (findTimestampOfLatestSnapshot list)) of
         [] -> Version $ createVersionNumberByNumberOfDimensions 0
         (x:xs) -> artifactToVersion x
instance FindVersionOfLatestSnapshot ArtifactTree where
     findVersionOfLatestSnapshot rTree = case (searchArtifactTree rTree ( findTimestampOfLatestSnapshot rTree ) ) of
         [] -> Version $ createVersionNumberByNumberOfDimensions 0 
         (x:xs) -> artifactToVersion x
         
class GetArtifactOfLatestSnapshot searchStructure where
     getArtifactOfLatestSnapshot :: searchStructure -> Artifact
    
instance GetArtifactOfLatestSnapshot ArtifactTreeList where
     getArtifactOfLatestSnapshot list = case (searchArtifactTreeList list (findTimestampOfLatestSnapshot list)) of
         [] -> initialArtifact 0
         (x:xs) -> x

instance GetArtifactOfLatestSnapshot ArtifactTree where
     getArtifactOfLatestSnapshot aTree = case (searchArtifactTree aTree ( findTimestampOfLatestSnapshot aTree ) ) of
         [] -> initialArtifact 0
         (x:xs) -> x

class FindVersionOfLatestExperimentalSnapshot a where
        findVersionOfLatestExperimentalSnapshot :: a -> Version

instance FindVersionOfLatestExperimentalSnapshot ArtifactTree where
        findVersionOfLatestExperimentalSnapshot (RoseTree artifact list) = case (isExperimentalSnapshot (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestExperimentalSnapshot list )
                False -> findVersionOfLatestExperimentalSnapshot list  

instance FindVersionOfLatestExperimentalSnapshot ArtifactTreeList where
        findVersionOfLatestExperimentalSnapshot [] = Version $ createVersionNumberByNumberOfDimensions 0
        findVersionOfLatestExperimentalSnapshot (x:xs) = max ( findVersionOfLatestExperimentalSnapshot x ) (findVersionOfLatestExperimentalSnapshot xs) 

class FindVersionOfLatestReleaseSnapshot a where
        findVersionOfLatestReleaseSnapshot :: a -> Version

instance FindVersionOfLatestReleaseSnapshot ArtifactTree where
        findVersionOfLatestReleaseSnapshot (RoseTree artifact list) = case (isReleaseSnapshot (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestReleaseSnapshot list )
                False -> findVersionOfLatestReleaseSnapshot list  

instance FindVersionOfLatestReleaseSnapshot ArtifactTreeList where
        findVersionOfLatestReleaseSnapshot [] = Version $ createVersionNumberByNumberOfDimensions 0
        findVersionOfLatestReleaseSnapshot (x:xs) = max ( findVersionOfLatestReleaseSnapshot x ) (findVersionOfLatestReleaseSnapshot xs) 

class FindVersionOfLatestSupportSnapshot a where
        findVersionOfLatestSupportSnapshot :: a -> Version

instance FindVersionOfLatestSupportSnapshot ArtifactTree where
        findVersionOfLatestSupportSnapshot (RoseTree artifact list) = case (isReleaseSnapshot (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestSupportSnapshot list )
                False -> findVersionOfLatestSupportSnapshot list  

instance FindVersionOfLatestSupportSnapshot ArtifactTreeList where
        findVersionOfLatestSupportSnapshot [] = Version $ createVersionNumberByNumberOfDimensions 0
        findVersionOfLatestSupportSnapshot (x:xs) = max ( findVersionOfLatestSupportSnapshot x ) (findVersionOfLatestSupportSnapshot xs) 

class FindVersionOfLatestReleaseBranch a where
        findVersionOfLatestReleaseBranch :: a -> Version

instance FindVersionOfLatestReleaseBranch ArtifactTree where
        findVersionOfLatestReleaseBranch (RoseTree artifact list) = case (isReleaseBranch (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestReleaseBranch list )
                False -> findVersionOfLatestReleaseBranch list  

instance FindVersionOfLatestReleaseBranch ArtifactTreeList where
        findVersionOfLatestReleaseBranch [] = Version $ createVersionNumberByNumberOfDimensions 0
        findVersionOfLatestReleaseBranch (x:xs) = max ( findVersionOfLatestReleaseBranch x ) (findVersionOfLatestReleaseBranch xs) 

class FindVersionOfLatestSupportBranch a where
        findVersionOfLatestSupportBranch :: a -> Version

instance FindVersionOfLatestSupportBranch ArtifactTree where
        findVersionOfLatestSupportBranch (RoseTree artifact list) = case (isSupportBranch (artifactToVersion artifact) ) of
                True -> max (artifactToVersion artifact) (findVersionOfLatestSupportBranch list )
                False -> findVersionOfLatestSupportBranch list

instance FindVersionOfLatestSupportBranch ArtifactTreeList where
        findVersionOfLatestSupportBranch [] = Version $ createVersionNumberByNumberOfDimensions 0
        findVersionOfLatestSupportBranch (x:xs) = max ( findVersionOfLatestSupportBranch x ) (findVersionOfLatestSupportBranch xs) 

class FindParentArtifact a where 
    findParentArtifact :: a -> Artifact -> Artifact

instance FindParentArtifact ArtifactTree where
    findParentArtifact (  RoseTree _ [] )  _ = initialArtifact 0
    findParentArtifact (  RoseTree parentArtifact list ) artifact = case (searchArtifactTreeChildren list artifact) of
        True -> parentArtifact
        False -> findParentArtifact list artifact
        
instance FindParentArtifact ArtifactTreeList where
    findParentArtifact [] _ = initialArtifact 0 
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
    generateSnapshot :: a -> ArtifactTree -> ArtifactTree

instance GenerateSnapshot Artifact where
    generateSnapshot artifact@(Artifact (Left (Branch _ _ _ ))) aTree =
        treeInsert aTree artifact 
            (liftSnapshot $ Snapshot ( (getArtifactTimestamp latestArtifact) + 1 ) (generateNewVersion (getArtifactVersion latestArtifact )) (artifactToDocument artifact)) 
        where latestArtifact = getArtifactOfLatestSnapshot aTree
    generateSnapshot artifact@(Artifact (Right (Snapshot _ _ _ ))) aTree = aTree


generateExperimentalVersionNumberFrom :: BranchName -> ArtifactTree -> Version
generateExperimentalVersionNumberFrom branchName aTree = incrementedVersion
        where 
                branch = searchArtifactTree aTree branchName !! 0
                branchVersion = getArtifactVersion branch
                latestVersion = case (isExperimentalBranch branchVersion) of 
                        True ->  (findVersionOfLatestExperimentalSnapshot aTree)
                        False -> case (isReleaseBranch branchVersion) of
                                True -> (findVersionOfLatestReleaseSnapshot aTree)
                                False -> case (isSupportBranch branchVersion) of
                                        True -> (findVersionOfLatestSupportSnapshot aTree)
                                        False -> Version $ createVersionNumberByNumberOfDimensions 0
                incrementedVersion = case (isInitial latestVersion) of
                        True -> generateNewExperimentalSnapshot branchVersion
                        False -> case (isExperimentalBranch branchVersion) of
                                True -> increment latestVersion
                                False -> case (isReleaseBranch branchVersion) of 
                                        True -> generateNewReleaseSnapshot latestVersion
                                        False -> case (isSupportBranch branchVersion) of
                                                True -> generateNewSupportSnapshot latestVersion
                                                False -> latestVersion

{-generateReleaseVersionNumberFrom :: BranchName -> ArtifactTree -> Version-}
{-generateReleaseVersionNumberFrom branchName aTree = incrementedVersion-}
        {-where -}
                {-branch = searchArtifactTree aTree branchName !! 0-}
                {-branchVersion = getArtifactVersion branch-}
                {-latestVersion = case (isExperimentalBranch branchVersion) of -}
                        {-True ->  (findVersionOfLatestExperimentalSnapshot aTree)-}
                        {-False -> case (isReleaseBranch branchVersion) of-}
                                {-True -> (findVersionOfLatestReleaseSnapshot aTree)-}
                                {-False -> case (isSupportBranch branchVersion) of-}
                                        {-True -> (findVersionOfLatestSupportSnapshot aTree)-}
                                        {-False -> Version $ createVersionNumberByNumberOfDimensions 0-}
                {-incrementedVersion = case (isInitialVersion latestVersion) of-}
                        {-True -> freezeReleaseVersion branchVersion-}
                        {-False -> case (isExperimentalBranch branchVersion) of-}
                                {-True -> increment latestVersion-}
                                {-False -> case (isReleaseBranch branchVersion) of -}
                                        {-True -> incrementReleaseNumberForVersion latestVersion-}
                                        {-False -> case (isSupportBranch branchVersion) of-}
                                                {-True -> incrementSupportNumberForVersion latestVersion-}
                                                {-False -> latestVersion-}

instance GenerateSnapshot BranchName where
    generateSnapshot branchName aTree = treeInsert aTree toBranchArtifact (liftSnapshot $ Snapshot ( (getArtifactTimestamp latestArtifact) + 1 ) newVersion (artifactToDocument toBranchArtifact)) 
        where 
           toBranchArtifact = searchArtifactTree aTree branchName !! 0
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

class GenerateBranch a where
    generateBranchFromSnapshot :: a -> BranchName -> ArtifactTree -> ArtifactTree 
    generateBranchFromBranch :: a -> BranchName -> ArtifactTree -> ArtifactTree 
    generateBranch :: a -> BranchName -> ArtifactTree -> ArtifactTree 

{-instance GenerateBranch Version where-}
    {-generateBranch version branchName aTree = treeInsert aTree snapshot branch-}
        {-where-}
            {-snapshot = searchArtifactTree aTree version !! 0-}
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
            artifact = searchArtifactTree aTree version !! 0
            
    generateBranchFromSnapshot versionString branchName aTree = treeInsert aTree artifact branch
        where
            version = stringToVersion versionString
            artifact = searchArtifactTree aTree version !! 0
            parentBranch = findParentArtifact aTree artifact
            versionOfParentBranch = getArtifactVersion parentBranch
            branch = liftBranch $ Branch branchName ( versionOfParentBranch ) (artifactToDocument artifact)

{-generateBranchFromBranch :: BranchName -> BranchName -> ArtifactTree -> ArtifactTree -}
    generateBranchFromBranch fromBranchName toBranchName aTree = treeInsert treeWithArtifact artifact branch
        where
            treeWithArtifact = generateSnapshot fromBranchName aTree
            version = findVersionOfLatestExperimentalSnapshot treeWithArtifact 
            artifact = searchArtifactTree treeWithArtifact version !! 0
            parentBranch = findParentArtifact treeWithArtifact artifact
            versionOfParentBranch = getArtifactVersion parentBranch
            branch = liftBranch $ Branch toBranchName ( versionOfParentBranch ) (artifactToDocument artifact)
                    
createExperimentalBranch = generateBranch

{-treeDimensionsMoreThanTwo :: ArtifactTree -> Bool-}
{-treeDimensionsMoreThanTwo aTree = getNumberOfDimensions ( searchArtifactTree aTree (Version $ VersionCompound $ NumberPlaceholder) !! 0 ) >= Number 2-}

{-treeDimensionsMoreThanThree :: ArtifactTree -> Bool-}
{-treeDimensionsMoreThanThree aTree = getNumberOfDimensions ( searchArtifactTree aTree (Version $ VersionCompound $ NumberPlaceholder) !! 0 ) >= Number 3-}

createReleaseBranch :: BranchName -> ArtifactTree -> ArtifactTree 
createReleaseBranch branchName aTree = treeInsert treeWithArtifact artifact branch
        where
                treeWithArtifact = generateSnapshot branchName aTree
                version = findVersionOfLatestExperimentalSnapshot treeWithArtifact 
                artifact = searchArtifactTree treeWithArtifact version !! 0
                parentBranch = findParentArtifact treeWithArtifact artifact
                latestReleaseVersion = findVersionOfLatestReleaseBranch treeWithArtifact
                releaseVersion = generateNewReleaseSnapshot latestReleaseVersion 
                branch = liftBranch $ Branch (show releaseVersion) ( releaseVersion ) (artifactToDocument artifact)

createSupportBranch:: BranchName -> ArtifactTree -> ArtifactTree 
createSupportBranch branchName aTree = treeInsert treeWithArtifact artifact branch
        where
                treeWithArtifact = generateSnapshot branchName aTree
                version = findVersionOfLatestExperimentalSnapshot treeWithArtifact 
                artifact = searchArtifactTree treeWithArtifact version !! 0
                parentBranch = findParentArtifact treeWithArtifact artifact
                latestSupportVersion = findVersionOfLatestSupportBranch treeWithArtifact
                supportVersion = generateNewSupportSnapshot latestSupportVersion 
                branch = liftBranch $ Branch (show supportVersion) ( supportVersion ) (artifactToDocument artifact)


class EditArtifactTree a where
        editArtifactTree :: BranchName -> DocumentOrDirectory -> a -> a

instance EditArtifactTree ArtifactTree where 
        editArtifactTree name newDD aTree@( RoseTree artifact [] ) = case (getArtifactName artifact == name) of 
                True -> (RoseTree (editArtifact newDD artifact) [] )
                False -> aTree
        editArtifactTree name newDD ( RoseTree artifact (x:xs) ) = case (getArtifactName artifact == name) of
                True -> (RoseTree (editArtifact newDD artifact) (editArtifactTree name newDD (x:xs)))
                False -> (RoseTree artifact (editArtifactTree name newDD (x:xs)))

instance EditArtifactTree ArtifactTreeList where 
    editArtifactTree _ _ []= []
    editArtifactTree branchName newDD (x:xs) = (editArtifactTree branchName newDD x ) : (editArtifactTree branchName newDD xs )

instance DimensionOperations ArtifactTree where 
    appendDimension (RoseTree artifact list ) = RoseTree (appendDimension artifact) (appendDimension list)
    getNumberOfDimensions aTree = getNumberOfDimensions (getArtifactVersion (searchArtifactTree aTree (Version $ VersionNumber [Nothing]) !! 0))
    
instance DimensionOperations ArtifactTreeList where 
    appendDimension [] = []
    appendDimension (x:xs) = ( appendDimension x ) : (appendDimension xs)

{-editArtifact :: BranchName -> Document -> ArtifactTree -> ArtifactTree -}
{-editArtifact branchName document aTree = searchArtifactTree aTree branchName -}

-- ARTIFACT TREE: APPLICATION OF DOCUMENT OPERATIONS -- 

-- updateArtifactTree :: ArtifactTree -> ArtifactTreeOperation -> ArtifactTree
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeEdit artifact@(Snapshot _ _ _) documentContent ) = aTree error "Snapshots are not editable"
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeEdit artifact@(Branch branchName version contents ) documentContent ) = treeUpdate aTree artifact ( Branch branchName version documentContent )
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateSnapshot artifact@(Snapshot _ _ _) ) = aTree
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateSnapshot artifact@(Branch _ _ _) ) = generateSnapshot aTree artifact
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateBranch artifact@(Snapshot _ _ document) branchName ) = treeInsert aTree artifact ( Branch branchName (generateNewVersion ( getArtifactVersion ( findParentArtifact aTree artifact ) ) ) document ) 
-- updateArtifactTree (ArtifactTree aTree _ ) ( ArtifactTreeCreateBranch artifact@(Branch _ _ document ) branchName ) = treeInsert newTree ( getArtifactOfLatestSnapshot newTree ) ( Branch branchName (generateNewVersion ( getArtifactVersion artifact ) ) document ) where newTree = (generateSnapshot aTree artifact) 

-- HELPER FUNCTIONS --

displayArtifactTree t = putStrLn $ drawVerticalTree (artifactTreeToStringTree t)

-- displayRepresentationsOfArtifactTree :: ArtifactTree -> IO ()
-- displayRepresentationsOfArtifactTree (ArtifactTree aTree _ ) = putStrLn $ (drawVerticalTree (artifactTreeToStringTree aTree) ++ "\n" 
--    ++ drawVerticalTree ( versionTreeToStringTree (artifactTreeToVersionTree aTree)) ++ "\n" 
--    ++ drawVerticalTree ( artifactTreeToAllowedChangesTree ( aTree))) ++ "\n"
--    ++ drawVerticalTree ( artifactListToPlatformsTree ( aTree ) (deploy aTree deploymentRules platformDB ) ) ++ "\n"
