{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module ArtifactTree where

import Data.Tree
import Data.Tree.Pretty
import Data.Time
import Data.Time.Clock.POSIX

import Artifact
import RoseTree
import Version
import VersionNumber
import VersionTree

import qualified Data.Aeson as JSON
import qualified Data.Text as T

type ArtifactTree = RoseTree Artifact
type ArtifactTreeList = [ArtifactTree]

-- ARTIFACT TREE OPERATIONS --

-- data ArtifactTreeOperation  = First
                            -- | Last
                            -- | Previous
                            -- | Next
                            -- | Parent -- do we really need it here?
                            -- | Children -- do we really need it here?

-- INITIAL ARTIFACT DEFINITIONS --

initialArtifactTree :: ArtifactTree
initialArtifactTree = RoseTree ( Snapshot 0 (Version (Number 0) ) ( Document "" "") ) 
    [ RoseTree (Branch "" ( Version NumberPlaceholder ) ( Document "" "") ) [] ]
    
initialArtifact :: Artifact
initialArtifact = Snapshot 0 ( initialVersion ) ( Document "" "" )

class IsInitialArtifact a where
    isInitialArtifact :: a -> Bool

instance IsInitialArtifact Artifact where
    isInitialArtifact a = case a of 
        (Snapshot _ ( Version ( Number 0 ) ) _ ) -> True
        _ -> False

instance IsInitialArtifact ArtifactTree where
    isInitialArtifact a = case a of 
        ( RoseTree (Snapshot _ (Version (Number 0) ) _ ) [ RoseTree ( Branch _ ( Version NumberPlaceholder ) _ ) [] ] ) -> True
        _ -> False

-- ARTIFACT TREE CONVERSION TO STRING --

artifactListToStringTreeList :: ArtifactTreeList -> StringTreeList
artifactListToStringTreeList [] = []
artifactListToStringTreeList (x:[]) = [artifactTreeToStringTree x]
artifactListToStringTreeList (x:xs) = (artifactTreeToStringTree x):(artifactListToStringTreeList xs) 

artifactTreeToStringTree :: ArtifactTree -> StringTree
artifactTreeToStringTree (RoseTree artifact []) = Node (artifactToString artifact) []
artifactTreeToStringTree (RoseTree artifact (x:[])) = Node (artifactToString artifact) [ artifactTreeToStringTree x ]
artifactTreeToStringTree (RoseTree artifact (x:xs)) = Node (artifactToString artifact) ( (artifactTreeToStringTree x) : (artifactListToStringTreeList xs) )

-- ARTIFACT TREE CONVERSION TO ALLOWED CHANGES --

artifactListToAllowedChangesList :: ArtifactTreeList -> StringTreeList
artifactListToAllowedChangesList [] = []
artifactListToAllowedChangesList (x:[]) = [artifactTreeToAllowedChangesTree x]
artifactListToAllowedChangesList (x:xs) = (artifactTreeToAllowedChangesTree x):(artifactListToAllowedChangesList xs) 

artifactTreeToAllowedChangesTree :: ArtifactTree -> StringTree
artifactTreeToAllowedChangesTree (RoseTree artifact []) = Node (allowedChangesToString ( detectAllowedChanges artifact)) []
artifactTreeToAllowedChangesTree (RoseTree artifact (x:[])) = Node (allowedChangesToString ( detectAllowedChanges artifact)) [ artifactTreeToAllowedChangesTree x ]
artifactTreeToAllowedChangesTree (RoseTree artifact (x:xs)) = Node (allowedChangesToString ( detectAllowedChanges artifact)) ( (artifactTreeToAllowedChangesTree x) : (artifactListToAllowedChangesList xs) )

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
    searchArtifactTreeList (x:xs) timestamp = (searchArtifactTree x timestamp) ++ (searchArtifactTreeList xs timestamp)

instance SearchArtifactTreeList BranchName where
    searchArtifactTreeList [] _ = []
    searchArtifactTreeList (artifactTree:[]) name = searchArtifactTree artifactTree name
    searchArtifactTreeList (x:xs) name = (searchArtifactTree x name) ++ (searchArtifactTreeList xs name)

class SearchArtifactTree searchEntity where
    searchArtifactTree :: ArtifactTree -> searchEntity -> [Artifact] 

instance SearchArtifactTree Version where
    searchArtifactTree (RoseTree artifact [] ) version = case (artifactHasVersion artifact version) of 
        True -> [artifact]
        False -> []
    searchArtifactTree (RoseTree artifact (x:xs) ) version = (searchArtifactTree (RoseTree artifact []) version) ++ (searchArtifactTree x version) ++ (searchArtifactTreeList xs version)

instance SearchArtifactTree Artifact where
    searchArtifactTree (RoseTree artifact1 [] ) artifact2 = case (artifact1 == artifact2) of 
        True -> [artifact1]
        False -> []
    searchArtifactTree (RoseTree artifact1 (x:xs) ) artifact2 = (searchArtifactTree (RoseTree artifact1 []) artifact2) ++ (searchArtifactTree x artifact2) ++ (searchArtifactTreeList xs artifact2)

instance SearchArtifactTree Timestamp where
    searchArtifactTree (RoseTree artifact [] ) timestamp = case (getArtifactTimestamp artifact == timestamp) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( RoseTree artifact (x:xs) ) timestamp = (searchArtifactTree (RoseTree artifact []) timestamp) ++ (searchArtifactTree x timestamp) ++ (searchArtifactTreeList xs timestamp)

instance SearchArtifactTree BranchName where
    searchArtifactTree (RoseTree artifact [] ) name = case (getArtifactName artifact == name) of 
        True -> [artifact]
        False -> []
    searchArtifactTree ( RoseTree artifact (x:xs) ) name = (searchArtifactTree (RoseTree artifact []) name) ++ (searchArtifactTree x name) ++ (searchArtifactTreeList xs name)

-- FIRST DEPTH SEARCH --

class SearchArtifactTreeChildren a where
    searchArtifactTreeChildren :: ArtifactTreeList -> a -> Bool    

instance SearchArtifactTreeChildren Artifact where
    searchArtifactTreeChildren [] _ = False
    searchArtifactTreeChildren ((RoseTree artifact1 _):xs) artifact2 = (artifact1 == artifact2) || (searchArtifactTreeChildren xs artifact2)
    
instance SearchArtifactTreeChildren Version where
    searchArtifactTreeChildren [] _ = False
    searchArtifactTreeChildren ((RoseTree artifact _):xs) version = (artifactHasVersion artifact version) || (searchArtifactTreeChildren xs version)

-- ARTIFACT TREE LATEST ARTIFACT OPERATIONS --

currentTimeStamp = (round `fmap` getPOSIXTime)

class FindTimestampOfLatestSnapshot searchStructure where
    findTimestampOfLatestSnapshot :: searchStructure -> Timestamp
    
instance FindTimestampOfLatestSnapshot ArtifactTree where    
    findTimestampOfLatestSnapshot (RoseTree artifact [] ) = getArtifactTimestamp artifact
    findTimestampOfLatestSnapshot (RoseTree artifact1 list ) = case ((getArtifactTimestamp artifact1) > (findTimestampOfLatestSnapshot list)) of
        True -> getArtifactTimestamp artifact1
        False -> findTimestampOfLatestSnapshot list

instance FindTimestampOfLatestSnapshot ArtifactTreeList where
    findTimestampOfLatestSnapshot [] = 0
    findTimestampOfLatestSnapshot (x:[]) = findTimestampOfLatestSnapshot x
    findTimestampOfLatestSnapshot (x:xs) = case ((findTimestampOfLatestSnapshot x) > (findTimestampOfLatestSnapshot xs)) of
        True -> (findTimestampOfLatestSnapshot x)
        False -> (findTimestampOfLatestSnapshot xs)

class FindVersionOfLatestSnapshot searchStructure where
    findVersionOfLatestSnapshot :: searchStructure -> Version

instance FindVersionOfLatestSnapshot ArtifactTreeList where
    findVersionOfLatestSnapshot list = case (searchArtifactTreeList list (findTimestampOfLatestSnapshot list)) of
        [] -> initialVersion
        (x:xs) -> artifactToVersion x
    
instance FindVersionOfLatestSnapshot ArtifactTree where
    findVersionOfLatestSnapshot aTree = case (searchArtifactTree aTree ( findTimestampOfLatestSnapshot aTree )) of
        [] -> initialVersion
        (x:xs) -> artifactToVersion x

class GetArtifactOfLatestSnapshot searchStructure where
    getArtifactOfLatestSnapshot :: searchStructure -> Artifact
    
instance GetArtifactOfLatestSnapshot ArtifactTreeList where
    getArtifactOfLatestSnapshot list = case (searchArtifactTreeList list (findTimestampOfLatestSnapshot list)) of
        [] -> initialArtifact
        (x:xs) -> x

instance GetArtifactOfLatestSnapshot ArtifactTree where
    getArtifactOfLatestSnapshot aTree = case (searchArtifactTree aTree ( findTimestampOfLatestSnapshot aTree )) of
        [] -> initialArtifact
        (x:xs) -> x

class FindParentArtifact a where 
    findParentArtifact :: a -> Artifact -> Artifact

instance FindParentArtifact ArtifactTree where
    findParentArtifact ( RoseTree _ [] ) _ = initialArtifact
    findParentArtifact ( RoseTree parentArtifact list ) artifact = case (searchArtifactTreeChildren list artifact) of
        True -> parentArtifact
        False -> findParentArtifact list artifact
        
instance FindParentArtifact ArtifactTreeList where
    findParentArtifact [] _ = initialArtifact
    findParentArtifact (x:xs) artifact = case ( isInitialArtifact (findParentArtifact x artifact) ) of
        True -> findParentArtifact xs artifact
        False -> findParentArtifact x artifact

class FindParentArtifacts a where 
    findParentArtifacts :: a -> ArtifactList -> ArtifactList

instance FindParentArtifacts ArtifactTree where
    findParentArtifacts tree [] = []
    findParentArtifacts tree (x:xs) = ( [findParentArtifact tree x] ) ++ ( findParentArtifacts tree xs )
    
instance FindParentArtifacts ArtifactTreeList where
    findParentArtifacts list [] = []
    findParentArtifacts list (x:xs) = ( [findParentArtifact list x] ) ++ ( findParentArtifacts list xs )

-- ARTIFACT TREE: GENERATION OF NEW ARTIFACTS --

class GenerateSnapshot a where 
    generateSnapshot :: ArtifactTree -> a -> ArtifactTree

instance GenerateSnapshot Artifact where
    generateSnapshot aTree artifact@(Branch _ _ _) = treeInsert aTree artifact ( Snapshot ( ( getArtifactTimestamp latestArtifact ) + 1 ) (generateNewVersion ( getArtifactVersion latestArtifact ) ) ( artifactToDocument artifact ) ) where latestArtifact = getArtifactOfLatestSnapshot aTree
    generateSnapshot aTree artifact@(Snapshot _ _ _) = aTree

instance GenerateSnapshot ArtifactList where
    generateSnapshot aTree [] = aTree
    generateSnapshot aTree (x:xs) = generateSnapshot (generateSnapshot aTree x) xs

-- ARTIFACT TREE: APPLICATION OF DOCUMENT OPERATIONS -- 

updateArtifactTree :: ArtifactTree -> DocumentOperation -> ArtifactTree
updateArtifactTree aTree ( Edit artifact@(Snapshot _ _ _) documentContent ) = aTree -- error "Snapshots are not editable"
updateArtifactTree aTree ( Edit artifact@(Branch branchName version contents ) documentContent ) = treeUpdate aTree artifact ( Branch branchName version documentContent )
updateArtifactTree aTree ( CreateSnapshot artifact@(Snapshot _ _ _) ) = aTree
updateArtifactTree aTree ( CreateSnapshot artifact@(Branch _ _ _) ) = generateSnapshot aTree artifact
updateArtifactTree aTree ( CreateBranch artifact@(Snapshot _ _ document) branchName ) = treeInsert aTree artifact ( Branch branchName (generateNewVersion ( getArtifactVersion ( findParentArtifact aTree artifact ) ) ) document ) 
updateArtifactTree aTree ( CreateBranch artifact@(Branch _ _ document ) branchName ) = treeInsert newTree ( getArtifactOfLatestSnapshot newTree ) ( Branch branchName (generateNewVersion ( getArtifactVersion artifact ) ) document ) where newTree = (generateSnapshot aTree artifact) 

-- EXAMPLES

artifactTree1 :: ArtifactTree
artifactTree1 = RoseTree (Snapshot 1398980989 (Version (Number 0) ) ( Document "" "") ) []

sArtifactTree1 :: StringTree
sArtifactTree1 = Node "0" []

artifactTree2 :: ArtifactTree
artifactTree2 = RoseTree (Snapshot 1398980989 (Version (Number 0) ) ( Document "" "") ) 
    [ RoseTree (Branch "trunk" (Version NumberPlaceholder ) ( Document "document1" "") ) [] ]

sArtifactTree2 :: StringTree
sArtifactTree2 = Node "0" [ Node "trunk (X) " [] ]
    
artifactTree3 :: ArtifactTree
artifactTree3 = RoseTree (Snapshot 1398980989 (Version (Number 0) ) (Document "" "") ) 
    [ RoseTree (Branch "trunk" (Version NumberPlaceholder ) ( Document "document1" "latest_content") ) [ 
        RoseTree (Snapshot 1398980990 (Version (Number 1) ) ( Document "document1" "content1") ) [], 
        RoseTree (Snapshot 1398980991 (Version (Number 2) ) ( Document "document1" "content2") ) [], 
        RoseTree (Snapshot 1398980992 (Version (Number 3) ) ( Document "document1" "content3") ) [
            RoseTree (Branch "branch1" (Version NumberPlaceholder ) ( Document "document1" "content_branch1") ) [
                RoseTree (Snapshot 1398980993 (Version (Number 4) ) ( Document "document1" "content4") ) [], 
                RoseTree (Snapshot 1398980995 (Version (Number 6) ) ( Document "document1" "content6") ) [] 
            ],
            RoseTree (Branch "branch2" (Version NumberPlaceholder ) ( Document "document1" "content_branch2") ) [
                RoseTree (Snapshot 1398980999 (Version (Number 10) ) ( Document "document1" "content10") ) [] 
            ]
        ],
        RoseTree (Snapshot 1398980994 (Version (Number 5) ) ( Document "document1" "content5") ) [], 
        RoseTree (Snapshot 1398980996 (Version (Number 7) ) ( Document "document1" "content7") ) [
            RoseTree (Branch "branch3" (Version NumberPlaceholder ) ( Document "document1" "content_branch3") ) [
                RoseTree (Snapshot 1398980998 (Version (Number 9) ) ( Document "document1" "content9") ) [] 
            ]
        ], 
        RoseTree (Snapshot 1398980997 (Version (Number 8) ) ( Document "document1" "content8") ) []
    ] ]

t = artifactTree3

sArtifactTree3 :: StringTree
sArtifactTree3 = Node "0" [ Node "trunk (X) " 
    [
        Node "1" [],
        Node "2" [],
        Node "3" [
            Node "branch1 (X)" [
                Node "4" [],
                Node "6" []
            ], 
            Node "branch2 (X)" [
                Node "10" []
            ] 
        ],
        Node "5" [],
        Node "7" [
            Node "branch3 (X)" [
                Node "9" []
            ]
        ],
        Node "8" []
    ] ]

-- HELPER FUNCTIONS --

displayArtifactTree t = putStrLn $ drawVerticalTree (artifactTreeToStringTree t)

displayRepresentationsOfArtifactTree :: ArtifactTree -> IO ()
displayRepresentationsOfArtifactTree aTree = putStrLn $ (drawVerticalTree (artifactTreeToStringTree aTree) ++ "\n" 
    ++ drawVerticalTree ( versionTreeToStringTree (artifactTreeToVersionTree aTree)) ++ "\n" 
    ++ drawVerticalTree ( artifactTreeToAllowedChangesTree ( aTree))) ++ "\n"
--    ++ drawVerticalTree ( artifactListToPlatformsTree ( aTree ) (deploy aTree deploymentRules platformDB ) ) ++ "\n"