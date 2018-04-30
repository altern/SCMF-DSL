{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Repository where

import Data.Tree
import Data.Tree.Pretty
import Data.Maybe
import RoseTree
import VersionNumber
import MaturityLevel
import Version
import Document 
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Control.Applicative
import GHC.Generics (Generic)

type RepositoryNodeContent = String 
type Timestamp = String 
type RepositoryNodeName = String 

emptyRepositoryNodeContent :: RepositoryNodeContent
emptyRepositoryNodeContent = ""

data RepositoryNode = RepositoryNode {
    version :: Version, 
    content :: RepositoryNodeContent,
    timestamp :: Timestamp
}
    {-deriving (Eq, Show, JSON.FromJSON, JSON.ToJSON, Generic)-}
    deriving (Eq, Show)

instance JSON.ToJSON RepositoryNode where 
    toJSON (RepositoryNode version content timestamp) = 
        JSON.object ["version" JSON..= (toString version), 
                     "content" JSON..= content,
                     "timestamp" JSON..= timestamp]

instance JSON.FromJSON RepositoryNode where
    parseJSON (JSON.Object v) = 
        RepositoryNode <$> liftM stringToVersion (v JSON..: "version")
                       <*> v JSON..: "content"
                       <*> v JSON..: "timestamp"
    parseJSON _ = mzero

type Repository = RoseTree RepositoryNode 
type RepositoryList = [Repository]

initialRepository :: Repository
initialRepository = RoseTree (RepositoryNode (Version $ VersionNumber [Just 0] ) emptyRepositoryNodeContent "0")
  [ RoseTree (RepositoryNode initialVersion emptyRepositoryNodeContent "1") []]

-- VERSION TREE CONVERSION TO STRING --
instance ToString RepositoryNode where 
    toString (RepositoryNode version _ _) = if (not (isRevision version) )
      {-then (toString version) ++ "(" ++ documentName ++  ")"-}
      then (toString version) 
      else (toString version) 

instance VersionDetection RepositoryNode where
    isInitial (RepositoryNode version _ _) = isInitial version
    isZero (RepositoryNode version _ _) = isZero version
    isExperimentalBranch (RepositoryNode version _ _) = isExperimentalBranch version
    isReleaseBranch (RepositoryNode version _ _) = isReleaseBranch version
    isSupportBranch (RepositoryNode version _ _) = isSupportBranch version
    isExperimentalSnapshot (RepositoryNode version _ _) = isExperimentalSnapshot version
    isReleaseSnapshot (RepositoryNode version _ _) = isReleaseSnapshot version
    isSupportSnapshot (RepositoryNode version _ _) = isSupportSnapshot version
    isRevision (RepositoryNode version _ _) = isRevision version

toMaturityNode :: RepositoryNode -> RepositoryNode
toMaturityNode (RepositoryNode version content timestamp) = RepositoryNode (toMaturityVersion version) content timestamp

toNode :: RepositoryNode -> RepositoryNode
toNode (RepositoryNode version content timestamp) = RepositoryNode (toVersion version) content timestamp

getRepositoryNodeContent :: RepositoryNode -> RepositoryNodeContent  
getRepositoryNodeContent (RepositoryNode _ content _) = content

versionListToStringTreeList :: RepositoryList -> StringTreeList
versionListToStringTreeList [] = []
versionListToStringTreeList (x:[]) = [versionDocumentTreeToStringTree x]
versionListToStringTreeList (x:xs) = (versionDocumentTreeToStringTree x):(versionListToStringTreeList xs) 

versionDocumentTreeToStringTree :: Repository -> StringTree
versionDocumentTreeToStringTree (RoseTree num []) = Node (toString num) []
versionDocumentTreeToStringTree (RoseTree num (x:[])) = Node (toString num) [ versionDocumentTreeToStringTree x ]
versionDocumentTreeToStringTree (RoseTree num (x:xs)) = Node (toString num) ( (versionDocumentTreeToStringTree x) : (versionListToStringTreeList xs) )

versionListToStringTreeListWithSelected :: Version -> RepositoryList -> StringTreeList
versionListToStringTreeListWithSelected _ [] = []
versionListToStringTreeListWithSelected selectedVersion (x:[]) = [versionDocumentTreeToStringTreeWithSelected selectedVersion x]
versionListToStringTreeListWithSelected selectedVersion (x:xs) = (versionDocumentTreeToStringTreeWithSelected selectedVersion x):(versionListToStringTreeListWithSelected selectedVersion xs) 

versionDocumentTreeToStringTreeWithSelected :: Version -> Repository -> StringTree
versionDocumentTreeToStringTreeWithSelected selectedVersion (RoseTree num []) = 
      if hasVersion selectedVersion num 
      then Node ("[" ++ toString num ++ "]") [] 
      else Node (toString num) []
versionDocumentTreeToStringTreeWithSelected selectedVersion (RoseTree num (x:[])) = 
      if hasVersion selectedVersion num 
      then Node ("[" ++ toString num ++ "]") [ versionDocumentTreeToStringTreeWithSelected selectedVersion x ]
      else Node (toString num) [ versionDocumentTreeToStringTreeWithSelected selectedVersion x ]
versionDocumentTreeToStringTreeWithSelected selectedVersion (RoseTree num (x:xs)) = 
      if hasVersion selectedVersion num 
      then Node ("[" ++ toString num ++ "]") ( (versionDocumentTreeToStringTreeWithSelected selectedVersion x) : (versionListToStringTreeListWithSelected selectedVersion xs) )
      else Node (toString num) ( (versionDocumentTreeToStringTreeWithSelected selectedVersion x) : (versionListToStringTreeListWithSelected selectedVersion xs) )

class Find a where 
    findLatestVersion :: a -> Version
    findLatestSupportBranch :: a -> Version
    findLatestReleaseBranch :: a -> Version
    findLatestReleaseBranchWithParent :: Version -> a -> Version
    findLatestSupportSnapshot :: a -> Version
    findLatestReleaseSnapshot :: a -> Version
    findLatestRevision :: a -> Version
    findLatestExperimentalSnapshot :: a -> Version
    findLatestForParentVersion :: Version -> a -> Version
    findLatestForParentSupportBranch :: Version -> a -> Version
    findLatestForParentReleaseBranch :: Version -> a -> Version
    findLatestForParentSupportSnapshot :: Version -> a -> Version
    findLatestForParentReleaseSnapshot :: Version -> a -> Version
    findLatestForParentRevision :: Version -> a -> Version
    findLatestForParentExperimentalSnapshot :: Version -> a -> Version
    findAllSupportBranches :: a -> [Version]
    findAllSupportSnapshots :: a -> [Version]
    findAllReleaseBranches :: a -> [Version]
    findAllReleaseSnapshots :: a -> [Version]
    findAllRevisions :: a -> [Version]
    findAllMainlines :: a -> [Version]

instance Find Repository where    
    findLatestVersion ( RoseTree (RepositoryNode version _ _)  [] ) = version
    findLatestVersion ( RoseTree (RepositoryNode version1 _ _) list ) = if ( version1 > ( findLatestVersion list ) ) then version1 else findLatestVersion list
    findLatestSupportBranch (RoseTree (RepositoryNode version _ _) []) = if (isSupportBranch version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportBranch (RoseTree (RepositoryNode version _ _) list) = if (isSupportBranch version && (version > findLatestSupportBranch list)) then version else findLatestSupportBranch list
    findLatestReleaseBranch (RoseTree (RepositoryNode version _ _) []) = if (isReleaseBranch version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseBranch (RoseTree (RepositoryNode version _ _) list) = if (isReleaseBranch version && (version > findLatestReleaseBranch list)) then version else findLatestReleaseBranch list
    findLatestReleaseBranchWithParent parentVersion (RoseTree (RepositoryNode version _ _) []) = 
        if (isReleaseBranch version && parentVersion == (getParent version)) 
          then version 
          else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseBranchWithParent parentVersion (RoseTree (RepositoryNode version _ _) list) = 
        if (isReleaseBranch version 
            && parentVersion == (getParent version) 
            && (version > findLatestReleaseBranchWithParent parentVersion list)) 
          then version 
          else findLatestReleaseBranchWithParent parentVersion list
    findLatestSupportSnapshot (RoseTree (RepositoryNode version _ _) []) = if (isSupportSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportSnapshot (RoseTree (RepositoryNode version _ _) list) = if (isSupportSnapshot version && (version > findLatestSupportSnapshot list)) then version else findLatestSupportSnapshot list
    findLatestReleaseSnapshot (RoseTree (RepositoryNode version _ _) []) = if (isReleaseSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseSnapshot (RoseTree (RepositoryNode version _ _) list) = if (isReleaseSnapshot version && (version > findLatestReleaseSnapshot list)) then version else findLatestReleaseSnapshot list
    findLatestExperimentalSnapshot (RoseTree (RepositoryNode version _ _) []) = if (isExperimentalSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestExperimentalSnapshot (RoseTree (RepositoryNode version _ _) list) = if (isExperimentalSnapshot version && (version > findLatestExperimentalSnapshot list)) then version else findLatestExperimentalSnapshot list
    findLatestRevision = findLatestExperimentalSnapshot
    findLatestForParentVersion parentVersion (RoseTree (RepositoryNode version _ _) list) = if (parentVersion == version) then findLatestVersion list else findLatestForParentVersion parentVersion list
    findLatestForParentSupportBranch parentVersion (RoseTree (RepositoryNode version _ _) list) = if (parentVersion == version) then findLatestSupportBranch list else findLatestForParentSupportBranch parentVersion list
    findLatestForParentReleaseBranch parentVersion (RoseTree (RepositoryNode version _ _) list) = 
        if (parentVersion == version) 
          then findLatestReleaseBranch list 
          else findLatestForParentReleaseBranch parentVersion list
    findLatestForParentSupportSnapshot parentVersion (RoseTree (RepositoryNode version _ _) list) = if (parentVersion == version) then findLatestSupportSnapshot list else findLatestForParentSupportSnapshot parentVersion list
    findLatestForParentReleaseSnapshot parentVersion (RoseTree (RepositoryNode version _ _) list) = if (parentVersion == version) then findLatestReleaseSnapshot list else findLatestForParentReleaseSnapshot parentVersion list
    findLatestForParentExperimentalSnapshot parentVersion (RoseTree (RepositoryNode version _ _) list) = if (parentVersion == version) then findLatestExperimentalSnapshot list else findLatestForParentExperimentalSnapshot parentVersion list
    findLatestForParentRevision = findLatestForParentExperimentalSnapshot
    findAllSupportBranches (RoseTree (RepositoryNode version _ _) []) = if isSupportBranch version then [version] else []
    findAllSupportBranches (RoseTree (RepositoryNode version _ _) list) = if isSupportBranch version then [version] ++ findAllSupportBranches list else findAllSupportBranches list
    findAllSupportSnapshots (RoseTree (RepositoryNode version _ _) []) = if isSupportSnapshot version then [version] else []
    findAllSupportSnapshots (RoseTree (RepositoryNode version _ _) list) = if isSupportSnapshot version then [version] ++ findAllSupportSnapshots list else findAllSupportSnapshots list
    findAllReleaseBranches (RoseTree (RepositoryNode version _ _) []) = if isReleaseBranch version then [version] else []
    findAllReleaseBranches (RoseTree (RepositoryNode version _ _) list) = if isReleaseBranch version then [version] ++ findAllReleaseBranches list else findAllReleaseBranches list
    findAllReleaseSnapshots (RoseTree (RepositoryNode version _ _) []) = if isReleaseSnapshot version then [version] else []
    findAllReleaseSnapshots (RoseTree (RepositoryNode version _ _) list) = if isReleaseSnapshot version then [version] ++ findAllReleaseSnapshots list else findAllReleaseSnapshots list
    findAllRevisions (RoseTree (RepositoryNode version _ _) []) = if isRevision version then [version] else []
    findAllRevisions (RoseTree (RepositoryNode version _ _) list) = if isRevision version then [version] ++ findAllRevisions list else findAllRevisions list
    findAllMainlines (RoseTree (RepositoryNode version _ _) []) = if isInitial version then [version] else []
    findAllMainlines (RoseTree (RepositoryNode version _ _) list) = if isInitial version then [version] ++ findAllMainlines list else findAllMainlines list

instance Find RepositoryList where
    findLatestVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestVersion (x:[]) = findLatestVersion x
    findLatestVersion (x:xs) = if ((findLatestVersion x) > (findLatestVersion xs)) then (findLatestVersion x) else (findLatestVersion xs)
    findLatestSupportBranch [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportBranch (x:[]) = findLatestSupportBranch x
    findLatestSupportBranch (x:xs) = if ((findLatestSupportBranch x) > (findLatestSupportBranch xs)) then (findLatestSupportBranch x) else (findLatestSupportBranch xs)
    findLatestReleaseBranch [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseBranch (x:[]) = findLatestReleaseBranch x
    findLatestReleaseBranch (x:xs) = if ((findLatestReleaseBranch x) > (findLatestReleaseBranch xs)) then (findLatestReleaseBranch x) else (findLatestReleaseBranch xs)
    findLatestReleaseBranchWithParent parentVersion (x:[]) = findLatestReleaseBranchWithParent parentVersion x
    findLatestReleaseBranchWithParent parentVersion (x:xs) = 
        if ((findLatestReleaseBranchWithParent parentVersion x) > (findLatestReleaseBranchWithParent parentVersion xs)) 
          then (findLatestReleaseBranchWithParent parentVersion x) 
          else (findLatestReleaseBranchWithParent parentVersion xs)
    findLatestSupportSnapshot [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportSnapshot (x:[]) = findLatestSupportSnapshot x
    findLatestSupportSnapshot (x:xs) = if ((findLatestSupportSnapshot x) > (findLatestSupportSnapshot xs)) then (findLatestSupportSnapshot x) else (findLatestSupportSnapshot xs)
    findLatestReleaseSnapshot [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseSnapshot (x:[]) = findLatestReleaseSnapshot x
    findLatestReleaseSnapshot (x:xs) = if ((findLatestReleaseSnapshot x) > (findLatestReleaseSnapshot xs)) then (findLatestReleaseSnapshot x) else (findLatestReleaseSnapshot xs)
    findLatestExperimentalSnapshot [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestExperimentalSnapshot (x:[]) = findLatestExperimentalSnapshot x
    findLatestExperimentalSnapshot (x:xs) = if ((findLatestExperimentalSnapshot x) > (findLatestExperimentalSnapshot xs)) then (findLatestExperimentalSnapshot x) else (findLatestExperimentalSnapshot xs)
    findLatestRevision = findLatestExperimentalSnapshot
    findLatestForParentVersion parentVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestForParentVersion parentVersion (x:[]) = findLatestForParentVersion parentVersion x
    findLatestForParentVersion parentVersion (x:xs) = 
        if ((findLatestForParentVersion parentVersion x) > (findLatestForParentVersion parentVersion xs)) 
        then (findLatestForParentVersion parentVersion x)
        else (findLatestForParentVersion parentVersion xs)
    findLatestForParentSupportBranch parentVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestForParentSupportBranch parentVersion (x:[]) = findLatestForParentSupportBranch parentVersion x
    findLatestForParentSupportBranch parentVersion (x:xs) = 
        if ((findLatestForParentSupportBranch parentVersion x) > (findLatestForParentSupportBranch parentVersion xs)) 
        then (findLatestForParentSupportBranch parentVersion x)
        else (findLatestForParentSupportBranch parentVersion xs)
    findLatestForParentReleaseBranch parentVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestForParentReleaseBranch parentVersion (x:[]) = findLatestForParentReleaseBranch parentVersion x
    findLatestForParentReleaseBranch parentVersion (x:xs) = 
        if ((findLatestForParentReleaseBranch parentVersion x) > (findLatestForParentReleaseBranch parentVersion xs)) 
        then (findLatestForParentReleaseBranch parentVersion x)
        else (findLatestForParentReleaseBranch parentVersion xs)
    findLatestForParentSupportSnapshot parentVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestForParentSupportSnapshot parentVersion (x:[]) = findLatestForParentSupportSnapshot parentVersion x
    findLatestForParentSupportSnapshot parentVersion (x:xs) = 
        if ((findLatestForParentSupportSnapshot parentVersion x) > (findLatestForParentSupportSnapshot parentVersion xs)) 
        then (findLatestForParentSupportSnapshot parentVersion x)
        else (findLatestForParentSupportSnapshot parentVersion xs)
    findLatestForParentReleaseSnapshot parentVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestForParentReleaseSnapshot parentVersion (x:[]) = findLatestForParentReleaseSnapshot parentVersion x
    findLatestForParentReleaseSnapshot parentVersion (x:xs) = 
        if ((findLatestForParentReleaseSnapshot parentVersion x) > (findLatestForParentReleaseSnapshot parentVersion xs)) 
        then (findLatestForParentReleaseSnapshot parentVersion x)
        else (findLatestForParentReleaseSnapshot parentVersion xs)
    findLatestForParentExperimentalSnapshot parentVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestForParentExperimentalSnapshot parentVersion (x:[]) = findLatestForParentExperimentalSnapshot parentVersion x
    findLatestForParentExperimentalSnapshot parentVersion (x:xs) = 
        if ((findLatestForParentExperimentalSnapshot parentVersion x) > (findLatestForParentExperimentalSnapshot parentVersion xs)) 
        then (findLatestForParentExperimentalSnapshot parentVersion x)
        else (findLatestForParentExperimentalSnapshot parentVersion xs)
    findLatestForParentRevision = findLatestForParentExperimentalSnapshot
    findAllSupportBranches [] = []
    findAllSupportBranches (x:[]) = findAllSupportBranches x
    findAllSupportBranches (x:xs) = findAllSupportBranches x ++ findAllSupportBranches xs
    findAllSupportSnapshots [] = []
    findAllSupportSnapshots (x:[]) = findAllSupportSnapshots x
    findAllSupportSnapshots (x:xs) = findAllSupportSnapshots x ++ findAllSupportSnapshots xs
    findAllReleaseBranches [] = []
    findAllReleaseBranches (x:[]) = findAllReleaseBranches x
    findAllReleaseBranches (x:xs) = findAllReleaseBranches x ++ findAllReleaseBranches xs
    findAllReleaseSnapshots [] = []
    findAllReleaseSnapshots (x:[]) = findAllReleaseSnapshots x
    findAllReleaseSnapshots (x:xs) = findAllReleaseSnapshots x ++ findAllReleaseSnapshots xs
    findAllRevisions [] = []
    findAllRevisions (x:[]) = findAllRevisions x
    findAllRevisions (x:xs) = findAllRevisions x ++ findAllRevisions xs
    findAllMainlines [] = []
    findAllMainlines (x:[]) = findAllMainlines x
    findAllMainlines (x:xs) = findAllMainlines x ++ findAllMainlines xs
    
class FindVersion a where 
    findParentVersion :: a -> Version -> Version
    findVersion :: a -> Version -> Version

instance FindVersion Repository where
    findParentVersion ( RoseTree _ [] ) _ = Version $ createVersionNumberByNumberOfDimensions 0
    findParentVersion ( RoseTree (RepositoryNode parentVersion _ _) list ) version = case (searchRepositoryChildren list version) of
        True -> parentVersion
        False -> findParentVersion list version
    findVersion (RoseTree (RepositoryNode version _ _) list) searchVersion = if (version == searchVersion) 
        then version
        else findVersion list searchVersion
        
instance FindVersion RepositoryList where
    findParentVersion [] _ = Version $ createVersionNumberByNumberOfDimensions 0
    findParentVersion (x:xs) version = case ( isInitial (findParentVersion x version) ) of
        True -> findParentVersion xs version
        False -> findParentVersion x version
    findVersion [] _ = Version $ createVersionNumberByNumberOfDimensions 0
    findVersion (x:xs) searchVersion = case ( isInitial (findVersion x searchVersion)) of
        True -> findVersion xs searchVersion
        False -> findVersion x searchVersion

class SearchRepositoryChildren a where
    searchRepositoryChildren :: RepositoryList -> a -> Bool    
    
instance SearchRepositoryChildren Version where
    searchRepositoryChildren [] _ = False
    searchRepositoryChildren ((RoseTree (RepositoryNode version1 _ _) _):xs) version2 = (version1 == version2) || (searchRepositoryChildren xs version2)

instance MakeDimensional RepositoryNode where
    makeNDimensional dim (RepositoryNode version content timestamp) = RepositoryNode (makeNDimensional dim version) content timestamp

instance MakeDimensional Repository where
    makeNDimensional dim (RoseTree (RepositoryNode version content timestamp) list) = RoseTree (RepositoryNode (makeNDimensional dim version) content timestamp) (makeNDimensional dim list)
    
instance MakeDimensional RepositoryList where
    makeNDimensional dim [] = []
    makeNDimensional dim (x:xs) = [makeNDimensional dim x] ++ (makeNDimensional dim xs)

instance DimensionOperations Repository where
    getNumberOfDimensions (RoseTree (RepositoryNode version _ _) list) = max (getNumberOfDimensions version) (getNumberOfDimensions list)
    getActualNumberOfDimensions (RoseTree (RepositoryNode version _ _) list) = max (getActualNumberOfDimensions version) (getActualNumberOfDimensions list)
    appendDimension (RoseTree (RepositoryNode version content timestamp) list) = RoseTree (RepositoryNode (appendDimension version) content timestamp) (appendDimension list) 

instance DimensionOperations RepositoryList where
    getNumberOfDimensions [] = 0
    getNumberOfDimensions (x:xs) = max (getNumberOfDimensions x) (getNumberOfDimensions xs)
    getActualNumberOfDimensions [] = 0
    getActualNumberOfDimensions (x:xs) = max (getActualNumberOfDimensions x) (getActualNumberOfDimensions xs)
    appendDimension [] = []
    appendDimension (x:xs) = [appendDimension x] ++ appendDimension xs

newRevision :: Version -> Repository -> Timestamp -> Repository 
newRevision searchVersion vTree timestamp = let
    document = getRepositoryNodeContentByVersion searchVersion vTree
    in if (isInitial searchVersion || isSupportBranch searchVersion || isReleaseBranch searchVersion) then
        (treeInsert 
          vTree 
          (RepositoryNode searchVersion document timestamp) 
          (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document timestamp)
        )
    else 
        vTree

hasVersion :: Version -> RepositoryNode -> Bool
hasVersion version1 (RepositoryNode version2 _ _) = version1 == version2

getRepositoryNodeContentByVersion :: Version -> Repository -> RepositoryNodeContent
getRepositoryNodeContentByVersion searchVersion vTree = 
    let searchRepositoryNode = (searchTree (hasVersion searchVersion) vTree)
    in (case searchRepositoryNode of
      Nothing -> emptyRepositoryNodeContent
      Just node -> getRepositoryNodeContent node
    )

editBranch :: Version -> String -> Repository -> Repository
editBranch searchVersion content vTree = 
    let searchRepositoryNode = (searchTree (hasVersion searchVersion) vTree)
    in (treeUpdate 
        vTree 
        (case searchRepositoryNode of
         Nothing -> RepositoryNode initialVersion emptyRepositoryNodeContent "0"
         Just node -> node
        )
        (RepositoryNode searchVersion content "")
    )

newReleaseBranch :: Version -> Repository -> Timestamp -> Repository
newReleaseBranch searchVersion vTree timestamp = 
    if (isInitial searchVersion || isSupportBranch searchVersion) then
        let document = getRepositoryNodeContentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> 
                (treeInsert 
                    vTree 
                    (fromJust $ searchTree (hasVersion searchVersion) vTree)
                    {-(RepositoryNode searchVersion document timestamp)-}
                    (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document timestamp)
                )
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestReleaseBranchWithParent searchVersion vTree
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) 
                $ generateNewReleaseBranch (if (isInitial previousVersion) then searchVersion else previousVersion)
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion document timestamp)
           )
    else 
        vTree

newSupportBranch :: Version -> Repository -> Timestamp -> Repository
newSupportBranch searchVersion vTree timestamp = 
    if (isInitial searchVersion) then
        let document = getRepositoryNodeContentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> 
                (treeInsert 
                    vTree 
                    (fromJust $ searchTree (hasVersion searchVersion) vTree)
                    {-(RepositoryNode searchVersion document timestamp)-}
                    (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document timestamp)
                )
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentSupportBranch searchVersion vTree
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) $ generateNewSupportBranch (if (isInitial previousVersion) then searchVersion else previousVersion)
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion document timestamp)
           )
    else 
        vTree

newReleaseSnapshot :: Version -> Repository -> Timestamp -> Repository
newReleaseSnapshot searchVersion vTree timestamp = 
    if (isReleaseBranch searchVersion) then
        let document = getRepositoryNodeContentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> 
                (treeInsert 
                    vTree 
                    (fromJust $ searchTree (hasVersion searchVersion) vTree)
                    {-(RepositoryNode searchVersion document timestamp)-}
                    (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document timestamp)
                )
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentReleaseSnapshot searchVersion vTree
            searchOrPreviousVersionNumber = getVersionNumber $ (if (isInitial previousVersion) 
                                                                then searchVersion 
                                                                else previousVersion)
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) 
                       $ generateNewReleaseSnapshot (MaturityVersion Test searchOrPreviousVersionNumber) 
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion document timestamp)
           )
    else 
        vTree

newSupportSnapshot :: Version -> Repository -> Timestamp -> Repository
newSupportSnapshot searchVersion vTree timestamp = 
    if (isSupportBranch searchVersion) then
        let document = getRepositoryNodeContentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) 
                  <$> (treeInsert 
                          vTree 
                          (fromJust $ searchTree (hasVersion searchVersion) vTree)
                          {-( RepositoryNode searchVersion document timestamp)-}
                          ( RepositoryNode (generateNewRevision (findLatestRevision vTree)) document timestamp)
                       )
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentSupportSnapshot searchVersion vTree
            searchOrPreviousVersionNumber = getVersionNumber (if (isInitial previousVersion) 
                                                              then searchVersion 
                                                              else previousVersion)
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) 
                       $ generateNewSupportSnapshot (MaturityVersion Dev searchOrPreviousVersionNumber) 
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion document timestamp)
           )
    else 
        vTree

promoteSnapshot :: Version -> Repository -> Timestamp -> Repository
promoteSnapshot promotedVersion vTree timestamp =
    if (isSupportSnapshot promotedVersion ) then
        let parentContent = getRepositoryNodeContentByVersion (getParent promotedVersion) vTree
            promotedContent = getRepositoryNodeContentByVersion promotedVersion vTree
            vTree1 = (makeNDimensional dimensions) 
                 <$> (treeInsert 
                        vTree 
                        (fromJust $ searchTree (hasVersion (getParent promotedVersion)) vTree)
                        {-( RepositoryNode (getParent promotedVersion) parentContent timestamp)-}
                        ( RepositoryNode (generateNewRevision (findLatestRevision vTree)) parentContent timestamp)
                      ) 
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentSupportSnapshot (getParent promotedVersion) vTree
            promotedVersionMaturity = getMaturity (findVersion vTree promotedVersion)
            promotedOrPrevious = (if (isInitial previousVersion) 
                                  then promotedVersion 
                                  else previousVersion)
            promotedOrPreviousVersionNumber = getVersionNumber promotedOrPrevious
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) 
                       $ if (promotedContent /= parentContent)
                         then promoteSupportVersion (MaturityVersion (getMaturity promotedOrPrevious) promotedOrPreviousVersionNumber) 
                         else (MaturityVersion (incrementSupport $ getMaturity promotedOrPrevious) promotedOrPreviousVersionNumber)
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion parentContent timestamp)
           )
    else if (isReleaseSnapshot promotedVersion ) then
        let parentContent = getRepositoryNodeContentByVersion (getParent promotedVersion) vTree
            promotedContent = getRepositoryNodeContentByVersion promotedVersion vTree
            vTree1 = (makeNDimensional dimensions) 
                 <$> (treeInsert 
                        vTree 
                        (fromJust $ searchTree (hasVersion (getParent promotedVersion)) vTree)
                        {-( RepositoryNode (getParent promotedVersion) parentContent timestamp)-}
                        ( RepositoryNode (generateNewRevision (findLatestRevision vTree)) parentContent timestamp)
                      )
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentReleaseSnapshot (getParent promotedVersion) vTree
            promotedVersionMaturity = getMaturity (findVersion vTree promotedVersion)
            promotedOrPrevious = (if (isInitial previousVersion) 
                                  then promotedVersion 
                                  else previousVersion)
            promotedOrPreviousVersionNumber = getVersionNumber promotedOrPrevious 
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) 
                       $ if (promotedContent /= parentContent) 
                         then promoteVersion (MaturityVersion (getMaturity promotedOrPrevious) promotedOrPreviousVersionNumber) 
                         else (MaturityVersion (increment $ getMaturity promotedOrPrevious) promotedOrPreviousVersionNumber)
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion parentContent timestamp)
           )
    else
        vTree

reSnapshot :: Version -> Repository -> Timestamp -> Repository
reSnapshot promotedVersion vTree timestamp =
    if (isSupportSnapshot promotedVersion ) then
        let document = getRepositoryNodeContentByVersion (getParent promotedVersion) vTree
            vTree1 = (makeNDimensional dimensions) 
                 <$> (treeInsert 
                        vTree 
                        (fromJust $ searchTree (hasVersion (getParent promotedVersion)) vTree)
                        {-( RepositoryNode (getParent promotedVersion) document timestamp)-}
                        ( RepositoryNode (generateNewRevision (findLatestRevision vTree)) document timestamp)
                      ) 
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentSupportSnapshot (getParent promotedVersion) vTree
            promotedVersionMaturity = getMaturity (findVersion vTree promotedVersion)
            promotedOrPrevious = (if (isInitial previousVersion) then promotedVersion else previousVersion)
            promotedOrPreviousVersionNumber = getVersionNumber promotedOrPrevious 
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) 
                       $ (MaturityVersion (getMaturity promotedOrPrevious) (generateNewVersion promotedOrPreviousVersionNumber) ) 
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion document timestamp)
           )
    else if (isReleaseSnapshot promotedVersion ) then
        let document = getRepositoryNodeContentByVersion (getParent promotedVersion) vTree
            vTree1 = (makeNDimensional dimensions) 
                 <$> (treeInsert 
                        vTree 
                        (fromJust $ searchTree (hasVersion (getParent promotedVersion)) vTree)
                        {-( RepositoryNode (getParent promotedVersion) document timestamp)-}
                        ( RepositoryNode (generateNewRevision (findLatestRevision vTree)) document timestamp)
                      )
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentReleaseSnapshot (getParent promotedVersion) vTree
            promotedVersionMaturity = getMaturity (findVersion vTree promotedVersion)
            promotedOrPrevious = (if (isInitial previousVersion) then promotedVersion else previousVersion)
            promotedOrPreviousVersionNumber = getVersionNumber promotedOrPrevious
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) 
                       $ (MaturityVersion (getMaturity promotedOrPrevious) (generateNewVersion promotedOrPreviousVersionNumber) )
        in (treeInsert 
              vTree1 
              (fromJust $ searchTree (hasVersion (findLatestRevision vTree1)) vTree1)
              (RepositoryNode newVersion document timestamp)
           )
    else
        vTree

{--- HELPER FUNCTIONS ---}
instance DisplayTree Repository where 
    displayTree t = putStrLn $ drawVerticalTree (versionDocumentTreeToStringTree t)

displayTreeWithSelected :: Version -> Repository -> IO ()
displayTreeWithSelected selectedVersion t = putStrLn $ drawVerticalTree $ versionDocumentTreeToStringTreeWithSelected selectedVersion t

displayRepository :: Repository -> Version -> Bool -> Bool -> IO ()
displayRepository repository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag = do
  if isInitial selectedVersion 
    then if displayRevisionsFlag 
      then if displayMaturityLevelsFlag 
        then displayTree $ fmap toMaturityNode repository
        else displayTree $ fmap toNode repository
      else if displayMaturityLevelsFlag 
        then displayTree $ filterTree isRevision $ fmap toMaturityNode repository
        else displayTree $ filterTree isRevision $ fmap toNode repository
  else if displayRevisionsFlag 
    then if displayMaturityLevelsFlag 
      then displayTreeWithSelected selectedVersion $ fmap toMaturityNode repository
      else displayTreeWithSelected selectedVersion $ fmap toNode repository
    else if displayMaturityLevelsFlag 
      then displayTreeWithSelected selectedVersion $ filterTree isRevision $ fmap toMaturityNode repository
      else displayTreeWithSelected selectedVersion $ filterTree isRevision $ fmap toNode repository

