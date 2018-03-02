{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Repository where

import Data.Tree
import Data.Tree.Pretty
import Data.Maybe
import RoseTree
import VersionNumber
import Version
import Document 
import Artifact
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

data RepositoryNode = RepositoryNode Version Document
    deriving (Eq, Show, JSON.FromJSON, JSON.ToJSON, Generic)
type Repository = RoseTree RepositoryNode
type RepositoryList = [Repository]

initialRepository :: Repository
initialRepository = RoseTree (RepositoryNode (Version $ VersionNumber [Just 0] ) emptyDocument)
  [ RoseTree (RepositoryNode initialVersion emptyDocument) []]

-- VERSION TREE CONVERSION TO STRING --
instance ToString RepositoryNode where 
    toString (RepositoryNode version (Document documentName documentContent)) = if (not (isRevision version) )
      {-then (toString version) ++ "(" ++ documentName ++  ")"-}
      then (toString version) 
      else (toString version) 

instance GetDocument RepositoryNode where
  getDocumentName (RepositoryNode version document) = getDocumentName document
  getDocumentContent (RepositoryNode version document) = getDocumentContent document

getDocument :: RepositoryNode -> Document  
getDocument (RepositoryNode version document) = document

versionListToStringTreeList :: RepositoryList -> StringTreeList
versionListToStringTreeList [] = []
versionListToStringTreeList (x:[]) = [versionDocumentTreeToStringTree x]
versionListToStringTreeList (x:xs) = (versionDocumentTreeToStringTree x):(versionListToStringTreeList xs) 

versionDocumentTreeToStringTree :: Repository -> StringTree
versionDocumentTreeToStringTree (RoseTree num []) = Node (toString num) []
versionDocumentTreeToStringTree (RoseTree num (x:[])) = Node (toString num) [ versionDocumentTreeToStringTree x ]
versionDocumentTreeToStringTree (RoseTree num (x:xs)) = Node (toString num) ( (versionDocumentTreeToStringTree x) : (versionListToStringTreeList xs) )

class FindLatest a where 
    findLatestVersion :: a -> Version
    findLatestSupportBranch :: a -> Version
    findLatestReleaseBranch :: a -> Version
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


instance FindLatest Repository where    
    findLatestVersion ( RoseTree (RepositoryNode version document)  [] ) = version
    findLatestVersion ( RoseTree (RepositoryNode version1 document) list ) = if ( version1 > ( findLatestVersion list ) ) then version1 else findLatestVersion list
    findLatestSupportBranch (RoseTree (RepositoryNode version document) []) = if (isSupportBranch version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportBranch (RoseTree (RepositoryNode version document) list) = if (isSupportBranch version && (version > findLatestSupportBranch list)) then version else findLatestSupportBranch list
    findLatestReleaseBranch (RoseTree (RepositoryNode version document) []) = if (isReleaseBranch version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseBranch (RoseTree (RepositoryNode version document) list) = if (isReleaseBranch version && (version > findLatestReleaseBranch list)) then version else findLatestReleaseBranch list
    findLatestSupportSnapshot (RoseTree (RepositoryNode version document) []) = if (isSupportSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportSnapshot (RoseTree (RepositoryNode version document) list) = if (isSupportSnapshot version && (version > findLatestSupportSnapshot list)) then version else findLatestSupportSnapshot list
    findLatestReleaseSnapshot (RoseTree (RepositoryNode version document) []) = if (isReleaseSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseSnapshot (RoseTree (RepositoryNode version document) list) = if (isReleaseSnapshot version && (version > findLatestReleaseSnapshot list)) then version else findLatestReleaseSnapshot list
    findLatestExperimentalSnapshot (RoseTree (RepositoryNode version document) []) = if (isExperimentalSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestExperimentalSnapshot (RoseTree (RepositoryNode version document) list) = if (isExperimentalSnapshot version && (version > findLatestExperimentalSnapshot list)) then version else findLatestExperimentalSnapshot list
    findLatestRevision = findLatestExperimentalSnapshot
    findLatestForParentVersion parentVersion (RoseTree (RepositoryNode version document) list) = if (parentVersion == version) then findLatestVersion list else findLatestForParentVersion parentVersion list
    findLatestForParentSupportBranch parentVersion (RoseTree (RepositoryNode version document) list) = if (parentVersion == version) then findLatestSupportBranch list else findLatestForParentSupportBranch parentVersion list
    findLatestForParentReleaseBranch parentVersion (RoseTree (RepositoryNode version document) list) = if (parentVersion == version) then findLatestReleaseBranch list else findLatestForParentReleaseBranch parentVersion list
    findLatestForParentSupportSnapshot parentVersion (RoseTree (RepositoryNode version document) list) = if (parentVersion == version) then findLatestSupportSnapshot list else findLatestForParentSupportSnapshot parentVersion list
    findLatestForParentReleaseSnapshot parentVersion (RoseTree (RepositoryNode version document) list) = if (parentVersion == version) then findLatestReleaseSnapshot list else findLatestForParentReleaseSnapshot parentVersion list
    findLatestForParentExperimentalSnapshot parentVersion (RoseTree (RepositoryNode version document) list) = if (parentVersion == version) then findLatestExperimentalSnapshot list else findLatestForParentExperimentalSnapshot parentVersion list
    findLatestForParentRevision = findLatestForParentExperimentalSnapshot
instance FindLatest RepositoryList where
    findLatestVersion [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestVersion (x:[]) = findLatestVersion x
    findLatestVersion (x:xs) = if ((findLatestVersion x) > (findLatestVersion xs)) then (findLatestVersion x) else (findLatestVersion xs)
    findLatestSupportBranch [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportBranch (x:[]) = findLatestSupportBranch x
    findLatestSupportBranch (x:xs) = if ((findLatestSupportBranch x) > (findLatestSupportBranch xs)) then (findLatestSupportBranch x) else (findLatestSupportBranch xs)
    findLatestReleaseBranch [] = Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseBranch (x:[]) = findLatestReleaseBranch x
    findLatestReleaseBranch (x:xs) = if ((findLatestReleaseBranch x) > (findLatestReleaseBranch xs)) then (findLatestReleaseBranch x) else (findLatestReleaseBranch xs)
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
    
class FindVersion a where 
    findParentVersion :: a -> Version -> Version
    findVersion :: a -> Version -> Version

instance FindVersion Repository where
    findParentVersion ( RoseTree _ [] ) _ = Version $ createVersionNumberByNumberOfDimensions 0
    findParentVersion ( RoseTree (RepositoryNode parentVersion document) list ) version = case (searchRepositoryChildren list version) of
        True -> parentVersion
        False -> findParentVersion list version
    findVersion (RoseTree (RepositoryNode version document) list) searchVersion = if (version == searchVersion) 
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
    searchRepositoryChildren ((RoseTree (RepositoryNode version1 document) _):xs) version2 = (version1 == version2) || (searchRepositoryChildren xs version2)

instance MakeDimensional RepositoryNode where
    makeNDimensional dim (RepositoryNode version document) = RepositoryNode (makeNDimensional dim version) document

instance MakeDimensional Repository where
    makeNDimensional dim (RoseTree (RepositoryNode version document) list) = RoseTree (RepositoryNode (makeNDimensional dim version) document) (makeNDimensional dim list)
    
instance MakeDimensional RepositoryList where
    makeNDimensional dim [] = []
    makeNDimensional dim (x:xs) = [makeNDimensional dim x] ++ (makeNDimensional dim xs)

instance DimensionOperations Repository where
    getNumberOfDimensions (RoseTree (RepositoryNode version document) list) = max (getNumberOfDimensions version) (getNumberOfDimensions list)
    getActualNumberOfDimensions (RoseTree (RepositoryNode version document) list) = max (getActualNumberOfDimensions version) (getActualNumberOfDimensions list)
    appendDimension (RoseTree (RepositoryNode version document) list) = RoseTree (RepositoryNode (appendDimension version) document) (appendDimension list) 

instance DimensionOperations RepositoryList where
    getNumberOfDimensions [] = 0
    getNumberOfDimensions (x:xs) = max (getNumberOfDimensions x) (getNumberOfDimensions xs)
    getActualNumberOfDimensions [] = 0
    getActualNumberOfDimensions (x:xs) = max (getActualNumberOfDimensions x) (getActualNumberOfDimensions xs)
    appendDimension [] = []
    appendDimension (x:xs) = [appendDimension x] ++ appendDimension xs

newRevision :: Version -> Repository -> Repository 
newRevision searchVersion vTree = let
    document = getDocumentByVersion searchVersion vTree
    in if (isInitial searchVersion || isSupportBranch searchVersion || isReleaseBranch searchVersion) then
        (treeInsert 
          vTree 
          (RepositoryNode searchVersion document) 
          (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document)
        )
    else 
        vTree

hasVersion :: Version -> RepositoryNode -> Bool
hasVersion version1 (RepositoryNode version2 _) = version1 == version2

getDocumentByVersion :: Version -> Repository -> Document
getDocumentByVersion searchVersion vTree = 
    let searchRepositoryNode = (searchTree (hasVersion searchVersion) vTree)
    in (case searchRepositoryNode of
      Nothing -> emptyDocument
      Just document -> getDocument document
    )

getNameByVersion :: Version -> Repository -> String
getNameByVersion searchVersion vTree = 
    let searchRepositoryNode = (searchTree (hasVersion searchVersion) vTree)
    in (case searchRepositoryNode of
      Nothing -> ""
      Just document -> getDocumentName document
    )

getContentByVersion :: Version -> Repository -> String
getContentByVersion searchVersion vTree = 
    let searchRepositoryNode = (searchTree (hasVersion searchVersion) vTree)
    in (case searchRepositoryNode of
      Nothing -> ""
      Just document -> getDocumentContent document
    )

editBranch :: Version -> String -> String -> Repository -> Repository
editBranch searchVersion name contents vTree = 
    let searchRepositoryNode = (searchTree (hasVersion searchVersion) vTree)
    in (treeUpdate 
        vTree 
        (case searchRepositoryNode of
         Nothing -> RepositoryNode initialVersion emptyDocument
         Just document -> document
        )
        (RepositoryNode searchVersion (Document name contents))
    )

newReleaseBranch :: Version -> Repository -> Repository
newReleaseBranch searchVersion vTree = 
    if (isInitial searchVersion || isSupportBranch searchVersion) then
        let document = getDocumentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> (treeInsert vTree (RepositoryNode searchVersion document) (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document))
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentReleaseBranch searchVersion vTree
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) $ generateNewReleaseBranch (if (isInitial previousVersion) then searchVersion else previousVersion)
        in (treeInsert 
              vTree1 
              (RepositoryNode (findLatestRevision vTree1) document)
              (RepositoryNode newVersion document)
           )
    else 
        vTree

newSupportBranch :: Version -> Repository -> Repository
newSupportBranch searchVersion vTree = 
    if (isInitial searchVersion) then
        let document = getDocumentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> (treeInsert vTree (RepositoryNode searchVersion document) (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document))
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentSupportBranch searchVersion vTree
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) $ generateNewSupportBranch (if (isInitial previousVersion) then searchVersion else previousVersion)
        in (treeInsert 
              vTree1 
              (RepositoryNode (findLatestRevision vTree1) document)
              (RepositoryNode newVersion document)
           )
    else 
        vTree

newReleaseSnapshot :: Version -> Repository -> Repository
newReleaseSnapshot searchVersion vTree = 
    if (isReleaseBranch searchVersion) then
        let document = getDocumentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> (treeInsert vTree (RepositoryNode searchVersion document) (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document))
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentReleaseSnapshot searchVersion vTree
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) $ generateNewReleaseSnapshot (if (isInitial previousVersion) then searchVersion else previousVersion) 
        in (treeInsert 
              vTree1 
              (RepositoryNode (findLatestRevision vTree1) document)
              (RepositoryNode newVersion document)
           )
    else 
        vTree

newSupportSnapshot :: Version -> Repository -> Repository
newSupportSnapshot searchVersion vTree = 
    if (isSupportBranch searchVersion) then
        let document = getDocumentByVersion searchVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> (treeInsert vTree (RepositoryNode searchVersion document ) (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document ))
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentSupportSnapshot searchVersion vTree
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) $ generateNewSupportSnapshot (if (isInitial previousVersion) then searchVersion else previousVersion) 
        in (treeInsert 
              vTree1 
              (RepositoryNode (findLatestRevision vTree1) document)
              (RepositoryNode newVersion document)
           )
    else 
        vTree

promoteSnapshot :: Version -> Repository -> Repository
promoteSnapshot promotedVersion vTree =
    if (isSupportSnapshot promotedVersion ) then
        let document = getDocumentByVersion promotedVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> (treeInsert vTree ( RepositoryNode (getParent promotedVersion) document ) (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document )) 
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentSupportSnapshot (getParent promotedVersion) vTree
            promotedVersionMaturity = getMaturity (findVersion vTree promotedVersion)
            promotedOrPreviousVersionNumber = getVersionNumber (if (isInitial previousVersion) then promotedVersion else previousVersion)
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) $ promoteSupportVersion (MaturityVersion promotedVersionMaturity promotedOrPreviousVersionNumber) 
        in (treeInsert 
              vTree1 
              (RepositoryNode (findLatestRevision vTree1) document)
              (RepositoryNode newVersion document)
           )
    else if (isReleaseSnapshot promotedVersion ) then
        let document = getDocumentByVersion promotedVersion vTree
            vTree1 = (makeNDimensional dimensions) <$> (treeInsert vTree ( RepositoryNode (getParent promotedVersion) document ) (RepositoryNode (generateNewRevision (findLatestRevision vTree)) document ))
            dimensions = max (getActualNumberOfDimensions vTree) (getActualNumberOfDimensions newVersion)
            previousVersion = findLatestForParentReleaseSnapshot (getParent promotedVersion) vTree
            promotedVersionMaturity = getMaturity (findVersion vTree promotedVersion)
            promotedOrPreviousVersionNumber = getVersionNumber (if (isInitial previousVersion) then promotedVersion else previousVersion)
            newVersion = makeNDimensional (getActualNumberOfDimensions vTree) $ promoteVersion (MaturityVersion promotedVersionMaturity promotedOrPreviousVersionNumber) 
        in (treeInsert 
              vTree1 
              (RepositoryNode (findLatestRevision vTree1) document)
              (RepositoryNode newVersion document)
           )
    else
        vTree

{--- HELPER FUNCTIONS ---}
instance DisplayTree Repository where 
    displayTree t = putStrLn $ drawVerticalTree (versionDocumentTreeToStringTree t)

vDocumentTree :: Repository 
vDocumentTree = RoseTree (RepositoryNode (Version $ VersionNumber [Just 0]) (Document "" ""))
                [RoseTree (RepositoryNode (Version $ VersionNumber [Nothing]) (Document "" "")) [
                  RoseTree (RepositoryNode (Version $ VersionNumber [Just 1]) (Document "" "")) [],
                  RoseTree (RepositoryNode (Version $ VersionNumber [Just 2]) (Document "" "")) []
                ]]

