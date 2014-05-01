module ArtifactTree where

import Data.Tree
import Data.Tree.Pretty

import Artifact
import RoseTree
import Version
import VersionNumber
import VersionTree

type ArtifactTree = RoseTree Artifact
type ArtifactTreeList = [ArtifactTree]


-- ARTIFACT TREE OPERATIONS --

data ArtifactTreeOperation  = First
                            | Last
                            | Previous
                            | Next
                            | Parent -- do we really need it here?
                            | Children -- do we really need it here?

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
artifactListToAllowedChangesList(x:[]) = [artifactTreeToAllowedChangesTree x]
artifactListToAllowedChangesList (x:xs) = (artifactTreeToAllowedChangesTree x):(artifactListToAllowedChangesList xs) 

artifactTreeToAllowedChangesTree :: ArtifactTree -> StringTree
artifactTreeToAllowedChangesTree (RoseTree artifact []) = Node (allowedChangesToString ( detectAllowedChanges artifact)) []
artifactTreeToAllowedChangesTree (RoseTree artifact (x:[])) = Node (allowedChangesToString ( detectAllowedChanges artifact)) [ artifactTreeToAllowedChangesTree x ]
artifactTreeToAllowedChangesTree (RoseTree artifact (x:xs)) = Node (allowedChangesToString ( detectAllowedChanges artifact)) ( (artifactTreeToAllowedChangesTree x) : (artifactListToAllowedChangesList xs) )

-- ARTIFACT TREE CONVERSION TO VERSION TREE --

artifactListToVersionList :: ArtifactTreeList -> VersionTreeList
artifactListToVersionList [] = []
artifactListToVersionList (x:[]) = [artifactTreeToVersionTree x]
artifactListToVersionList (x:xs) = (artifactTreeToVersionTree x):(artifactListToVersionList xs) 

artifactTreeToVersionTree :: ArtifactTree -> VersionTree
artifactTreeToVersionTree (RoseTree artifact []) = RoseTree (artifactToVersion artifact) []
artifactTreeToVersionTree (RoseTree artifact (x:[])) = RoseTree (artifactToVersion artifact) [ artifactTreeToVersionTree x ]
artifactTreeToVersionTree (RoseTree artifact (x:xs)) = RoseTree (artifactToVersion artifact) ( (artifactTreeToVersionTree x) : (artifactListToVersionList xs) )

-- ARTIFACT TREE INSERT -- 

-- artifactTreeInsert :: (Ord a) => RoseTree a -> a -> a -> RoseTree a
-- artifactTreeInsert (RoseTree elem (x:xs)) find insert
    -- | x == find = ( RoseTree elem ( [x] ++ [ RoseTree insert [] ] ++ xs ) )
    -- | x == find = ( RoseTree elem [] )
--    | (treeContains x elem) = treeInsert x find insert
--    | (listContains xs elem) = (RoseTree elem (listInsert xs (RoseTree insert [])))
--    | otherwise = error "Cannot find node to append after"

-- ARTIFACT TREE SEARCH --

-- instance (Eq a) => Eq ([ArtifactTree]) where
--    [] == []              = True
--    a == b                = a == b
--    _ == _                = False

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
    
instance SearchArtifactTreeList Integer where
    searchArtifactTreeList aTreeList timestamp = undefined

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


instance SearchArtifactTree Integer where
    searchArtifactTree aTree timestamp = undefined

-- ARTIFACT TREE: APPLICATION OF DOCUMENT OPERATIONS -- 

updateArtifactTree :: ArtifactTree -> DocumentOperation -> ArtifactTree
updateArtifactTree aTree (Edit artifact documentContent) = undefined
updateArtifactTree aTree (CreateSnapshot artifact) = undefined
updateArtifactTree aTree (CreateBranch artifact) = undefined

-- EXAMPLES

artifactTree1 :: ArtifactTree
artifactTree1 = RoseTree (Snapshot 12345 (Version (Number 0) ) ( Document "" "") ) []

sArtifactTree1 :: StringTree
sArtifactTree1 = Node "0" []

artifactTree2 :: ArtifactTree
artifactTree2 = RoseTree (Snapshot 12345 (Version (Number 0) ) ( Document "" "") ) 
    [ RoseTree (Branch "trunk" (Version NumberPlaceholder ) ( Document "document1" "") ) [] ]

sArtifactTree2 :: StringTree
sArtifactTree2 = Node "0" [ Node "trunk (X) " [] ]
    
artifactTree3 :: ArtifactTree
artifactTree3 = RoseTree (Snapshot 12345 (Version (Number 0) ) (Document "" "") ) 
    [ RoseTree (Branch "trunk" (Version NumberPlaceholder ) ( Document "document1" "latest_content") ) [ 
        RoseTree (Snapshot 12346 (Version (Number 1) ) ( Document "document1" "content1") ) [], 
        RoseTree (Snapshot 12347 (Version (Number 2) ) ( Document "document1" "content2") ) [], 
        RoseTree (Snapshot 12349 (Version (Number 3) ) ( Document "document1" "content3") ) [
            RoseTree (Branch "branch1" (Version NumberPlaceholder ) ( Document "document1" "content_branch1") ) [
                RoseTree (Snapshot 12350 (Version (Number 4) ) ( Document "document1" "content4") ) [], 
                RoseTree (Snapshot 12362 (Version (Number 6) ) ( Document "document1" "content6") ) [] 
            ],
            RoseTree (Branch "branch2" (Version NumberPlaceholder ) ( Document "document1" "content_branch2") ) [
                RoseTree (Snapshot 12372 (Version (Number 10) ) ( Document "document1" "content10") ) [] 
            ]
        ],
        RoseTree (Snapshot 12352 (Version (Number 5) ) ( Document "document1" "content5") ) [], 
        RoseTree (Snapshot 12364 (Version (Number 7) ) ( Document "document1" "content7") ) [
            RoseTree (Branch "branch3" (Version NumberPlaceholder ) ( Document "document1" "content_branch3") ) [
                RoseTree (Snapshot 12370 (Version (Number 9) ) ( Document "document1" "content9") ) [] 
            ]
        ], 
        RoseTree (Snapshot 12368 (Version (Number 8) ) ( Document "document1" "content8") ) []
    ] ]
    
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
displayRepresentationsOfArtifactTree aTree = putStrLn $ (drawVerticalTree (artifactTreeToStringTree aTree) ++ "\n" ++ drawVerticalTree ( versionTreeToStringTree (artifactTreeToVersionTree aTree)) ++ "\n" ++ drawVerticalTree ( artifactTreeToAllowedChangesTree ( aTree)))