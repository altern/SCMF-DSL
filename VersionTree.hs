{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module VersionTree where

import Data.Tree
import Data.Tree.Pretty

import RoseTree
import VersionNumber
import Version
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

type VersionTree = RoseTree Version
type VersionTreeList = [VersionTree]

-- VERSION TREE OPERATIONS --

-- data VersionTreeOperation  = First
                            -- | Last
                            -- | Previous
                            -- | Next
                            -- | Parent -- do we really need it here?
                            -- | Children -- do we really need it here?
                            -- | Merge -- ?

-- initialVersionTree :: VersionTree
-- initialVersionTree = RoseTree (Version (Number 0))  
--    [ RoseTree ( Version NumberPlaceholder ) [] ]

-- isInitialVersionTree :: VersionTree -> Bool
-- isInitialVersionTree vTree = case vTree of 
--    ( RoseTree (Version (Number 0)) [ RoseTree ( Version NumberPlaceholder ) [] ] ) -> True
--    _ -> False

-- VERSION TREE CONVERSION TO STRING --

versionListToStringTreeList :: VersionTreeList -> StringTreeList
versionListToStringTreeList [] = []
versionListToStringTreeList (x:[]) = [versionTreeToStringTree x]
versionListToStringTreeList (x:xs) = (versionTreeToStringTree x):(versionListToStringTreeList xs) 

versionTreeToStringTree :: VersionTree -> StringTree
versionTreeToStringTree (RoseTree num []) = Node (toString num) []
versionTreeToStringTree (RoseTree num (x:[])) = Node (toString num) [ versionTreeToStringTree x ]
versionTreeToStringTree (RoseTree num (x:xs)) = Node (toString num) ( (versionTreeToStringTree x) : (versionListToStringTreeList xs) )

-- TODO: implement toJSON and FromJSON for versionTrees
-- instance JSON.ToJSON VersionTree where
    -- toJSON (RoseTree version children) = 
        -- JSON.object [ T.pack "version" JSON..= (T.pack $ show version)]
        -- JSON.object [ T.pack "children" JSON..= (JSON.toJSON children)]

-- instance JSON.FromJSON VersionTree where
    -- parseJSON (JSON.Object vTree) = liftM stringToVersion ( vTree JSON..: T.pack "versionTree" )
    -- parseJSON _ = mzero

-- VERSION TREE CONVERSION TO ALLOWED CHANGES --

versionListToAllowedChangesList :: VersionTreeList -> StringTreeList
versionListToAllowedChangesList [] = []
versionListToAllowedChangesList(x:[]) = [versionTreeToAllowedChangesTree x]
versionListToAllowedChangesList (x:xs) = (versionTreeToAllowedChangesTree x):(versionListToAllowedChangesList xs) 

versionTreeToAllowedChangesTree :: VersionTree -> StringTree
versionTreeToAllowedChangesTree (RoseTree num []) = Node (allowedChangesToString ( detectAllowedChanges num)) []
versionTreeToAllowedChangesTree (RoseTree num (x:[])) = Node (allowedChangesToString ( detectAllowedChanges num)) [ versionTreeToAllowedChangesTree x ]
versionTreeToAllowedChangesTree (RoseTree num (x:xs)) = Node (allowedChangesToString ( detectAllowedChanges num)) ( (versionTreeToAllowedChangesTree x) : (versionListToAllowedChangesList xs) )

class VersionTreeAppend structure where
    versionTreeAppend :: structure -> structure

{-instance VersionTreeAppend VersionTreeList where -}
    {-versionTreeAppend [] = [RoseTree (Version (VersionCompound (Number 0)) []]-}
    {-versionTreeAppend v@((RoseTree (Version (VersionCompound (Number n))) []):[]) = v ++ [(RoseTree (Version (VersionCompound (Number (n + 1)))) [] )]-}
    {-versionTreeAppend v@((RoseTree (Version (VersionCompound NumberPlaceholder)) []):[]) = v-}
    {-versionTreeAppend ((RoseTree (Version (VersionCompound NumberPlaceholder)) x):[]) = ((RoseTree (Version (VersionCompound NumberPlaceholder)) (versionTreeAppend x)):[])-}
    {-versionTreeAppend (x:xs) = [x] ++ (versionTreeAppend xs)-}

{-instance VersionTreeAppend VersionTree where -}
    {-versionTreeAppend (RoseTree (Version (VersionCompound NumberPlaceholder)) list) = (RoseTree (Version (VersionCompound NumberPlaceholder)) (versionTreeAppend list))-}
    {-versionTreeAppend a@(RoseTree (Version (VersionCompound (Number n))) list) = a-}

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


instance FindLatest VersionTree where    
    findLatestVersion ( RoseTree version [] ) = version
    findLatestVersion ( RoseTree version1 list ) = if ( version1 > ( findLatestVersion list ) ) then version1 else findLatestVersion list
    findLatestSupportBranch (RoseTree version []) = if (isSupportBranch version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportBranch (RoseTree version list) = if (isSupportBranch version && (version > findLatestSupportBranch list)) then version else findLatestSupportBranch list
    findLatestReleaseBranch (RoseTree version []) = if (isReleaseBranch version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseBranch (RoseTree version list) = if (isReleaseBranch version && (version > findLatestReleaseBranch list)) then version else findLatestReleaseBranch list
    findLatestSupportSnapshot (RoseTree version []) = if (isSupportSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestSupportSnapshot (RoseTree version list) = if (isSupportSnapshot version && (version > findLatestSupportSnapshot list)) then version else findLatestSupportSnapshot list
    findLatestReleaseSnapshot (RoseTree version []) = if (isReleaseSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestReleaseSnapshot (RoseTree version list) = if (isReleaseSnapshot version && (version > findLatestReleaseSnapshot list)) then version else findLatestReleaseSnapshot list
    findLatestExperimentalSnapshot (RoseTree version []) = if (isExperimentalSnapshot version) then version else Version $ createVersionNumberByNumberOfDimensions 0
    findLatestExperimentalSnapshot (RoseTree version list) = if (isExperimentalSnapshot version && (version > findLatestExperimentalSnapshot list)) then version else findLatestExperimentalSnapshot list
    findLatestRevision = findLatestExperimentalSnapshot
    findLatestForParentVersion parentVersion (RoseTree version list) = if (parentVersion == version) then findLatestVersion list else findLatestForParentVersion parentVersion list
    findLatestForParentSupportBranch parentVersion (RoseTree version list) = if (parentVersion == version) then findLatestSupportBranch list else findLatestForParentSupportBranch parentVersion list
    findLatestForParentReleaseBranch parentVersion (RoseTree version list) = if (parentVersion == version) then findLatestReleaseBranch list else findLatestForParentReleaseBranch parentVersion list
    findLatestForParentSupportSnapshot parentVersion (RoseTree version list) = if (parentVersion == version) then findLatestSupportSnapshot list else findLatestForParentSupportSnapshot parentVersion list
    findLatestForParentReleaseSnapshot parentVersion (RoseTree version list) = if (parentVersion == version) then findLatestReleaseSnapshot list else findLatestForParentReleaseSnapshot parentVersion list
    findLatestForParentExperimentalSnapshot parentVersion (RoseTree version list) = if (parentVersion == version) then findLatestExperimentalSnapshot list else findLatestForParentExperimentalSnapshot parentVersion list
    findLatestForParentRevision = findLatestForParentExperimentalSnapshot
instance FindLatest VersionTreeList where
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
    
class FindParentVersion a where 
    findParentVersion :: a -> Version -> Version

instance FindParentVersion VersionTree where
    findParentVersion ( RoseTree _ [] ) _ = Version $ createVersionNumberByNumberOfDimensions 0
    findParentVersion ( RoseTree parentVersion list ) version = case (searchVersionTreeChildren list version) of
        True -> parentVersion
        False -> findParentVersion list version
        
instance FindParentVersion VersionTreeList where
    findParentVersion [] _ = Version $ createVersionNumberByNumberOfDimensions 0
    findParentVersion (x:xs) version = case ( isInitial (findParentVersion x version) ) of
        True -> findParentVersion xs version
        False -> findParentVersion x version

class SearchVersionTreeChildren a where
    searchVersionTreeChildren :: VersionTreeList -> a -> Bool    
    
instance SearchVersionTreeChildren Version where
    searchVersionTreeChildren [] _ = False
    searchVersionTreeChildren ((RoseTree version1 _):xs) version2 = (version1 == version2) || (searchVersionTreeChildren xs version2)

appendNewVersion :: VersionTree -> Version -> VersionTree 
appendNewVersion vTree version = treeInsert vTree version ( increment version ) 


instance MakeDimensional VersionTree where
    makeNDimensional dim (RoseTree version list) = RoseTree (makeNDimensional dim version) (makeNDimensional dim list)
    
instance MakeDimensional VersionTreeList where
    makeNDimensional dim [] = []
    makeNDimensional dim (x:xs) = [makeNDimensional dim x] ++ (makeNDimensional dim xs)

instance DimensionOperations VersionTree where
    getNumberOfDimensions (RoseTree version list) = max (getNumberOfDimensions version) (getNumberOfDimensions list)
    getActualNumberOfDimensions (RoseTree version list) = max (getActualNumberOfDimensions version) (getActualNumberOfDimensions list)
    appendDimension (RoseTree version list) = RoseTree (appendDimension version) (appendDimension list) 

instance DimensionOperations VersionTreeList where
    getNumberOfDimensions [] = 0
    getNumberOfDimensions (x:xs) = max (getNumberOfDimensions x) (getNumberOfDimensions xs)
    getActualNumberOfDimensions [] = 0
    getActualNumberOfDimensions (x:xs) = max (getActualNumberOfDimensions x) (getActualNumberOfDimensions xs)
    appendDimension [] = []
    appendDimension (x:xs) = [appendDimension x] ++ appendDimension xs

newReleaseBranch :: Version -> VersionTree -> VersionTree
newReleaseBranch searchVersion vTree = 
    if (isInitial searchVersion || isSupportBranch searchVersion) then
        let vTree1 = (makeNDimensional 2) <$> (treeInsert vTree searchVersion (generateNewRevision (findLatestRevision vTree)))
            previousVersion = findLatestForParentReleaseBranch searchVersion vTree
        in (treeInsert 
              vTree1 
              (findLatestRevision vTree1)
              (generateNewReleaseBranch (if (isInitial previousVersion) then searchVersion else previousVersion))
           )
    else 
        vTree

newSupportBranch :: Version -> VersionTree -> VersionTree
newSupportBranch searchVersion vTree = 
    if (isInitial searchVersion) then
        let vTree1 = (makeNDimensional 3) <$> (treeInsert vTree searchVersion (generateNewRevision (findLatestRevision vTree)))
            previousVersion = findLatestForParentSupportBranch searchVersion vTree
        in (treeInsert 
              vTree1 
              (findLatestRevision vTree1)
              (generateNewSupportBranch (if (isInitial previousVersion) then searchVersion else previousVersion))
           )
    else 
        vTree

newReleaseSnapshot :: Version -> VersionTree -> VersionTree
newReleaseSnapshot searchVersion vTree = 
    if (isReleaseBranch searchVersion) then
        let vTree1 = (makeNDimensional 2) <$> (treeInsert vTree searchVersion (generateNewRevision (findLatestRevision vTree)))
            previousVersion = findLatestForParentReleaseSnapshot searchVersion vTree
        in (treeInsert 
              vTree1 
              (findLatestRevision vTree1)
              (generateNewReleaseSnapshot (if (isInitial previousVersion) then searchVersion else previousVersion))
           )
    else 
        vTree

newSupportSnapshot :: Version -> VersionTree -> VersionTree
newSupportSnapshot searchVersion vTree = 
    if (isSupportBranch searchVersion) then
        let vTree1 = (makeNDimensional 3) <$> (treeInsert vTree searchVersion (generateNewRevision (findLatestRevision vTree)))
            previousVersion = findLatestForParentSupportSnapshot searchVersion vTree
        in (treeInsert 
              vTree1 
              (findLatestRevision vTree1)
              (generateNewSupportSnapshot (if (isInitial previousVersion) then searchVersion else previousVersion))
           )
    else 
        vTree

initialVersionTree :: VersionTree
initialVersionTree = RoseTree (Version $ VersionNumber [Just 0]) [ RoseTree initialVersion []]

-- EXAMPLES --

vTree1 :: VersionTree
vTree1 = RoseTree (Version $ VersionNumber [Nothing]) []

strTree1 :: StringTree
strTree1 = Node "X" []

vTree2 :: VersionTree
vTree2 = RoseTree (Version $ VersionNumber [Nothing]) [RoseTree (Version $ VersionNumber [Just 1]) []]

strTree2 :: StringTree
strTree2 = Node "X" [Node "1" []]

vTree3 :: VersionTree
vTree3 = RoseTree (Version $ VersionNumber [Nothing] )  [RoseTree (Version $ VersionNumber [Just 1]) [], RoseTree (Version $ VersionNumber [Just 2]) []]

strTree3 :: StringTree
strTree3 = Node "X" [Node "1" [], Node "2" []]

vTree4 :: VersionTree
vTree4 = RoseTree (Version $ VersionNumber [Nothing]) [
            RoseTree (Version $ VersionNumber [Just 1]) [
                RoseTree (Version $ VersionNumber [Nothing]) []
            ], 
            RoseTree (Version $ VersionNumber [Just 2]) [
                RoseTree (Version $ VersionNumber [Nothing]) []
            ],
            RoseTree (Version $ VersionNumber [Just 3]) [
                RoseTree (Version $ VersionNumber [Nothing]) []
            ],
            RoseTree (Version $ VersionNumber [Just 4]) [
                RoseTree (Version $ VersionNumber [Nothing]) []
            ]
         ]

vTree5 :: VersionTree 
vTree5 = RoseTree (Version $ VersionNumber [Nothing, Nothing, Nothing]) [
            RoseTree (Version $ VersionNumber [Nothing, Nothing, Just 1]) [
                RoseTree (Version $ VersionNumber [Just 1, Nothing, Nothing]) [
                    RoseTree (Version $ VersionNumber [Nothing, Nothing, Just 6]) [
                        RoseTree (Version $ VersionNumber [Just 1, Nothing, Just 0]) []
                    ]
                ]
            ],
            RoseTree (Version $ VersionNumber [Nothing, Nothing, Just 2]) [
                RoseTree (Version $ VersionNumber [Just 2, Nothing, Nothing]) [
                    RoseTree (Version $ VersionNumber [Nothing, Nothing, Just 4]) [
                        RoseTree (Version $ VersionNumber [Just 2, Just 0, Nothing]) []
                    ],
                    RoseTree (Version $ VersionNumber [Nothing, Nothing, Just 5]) [
                        RoseTree (Version $ VersionNumber [Just 2, Just 1, Nothing]) [
                            RoseTree (Version $ VersionNumber [Nothing, Nothing, Just 7]) [
                                RoseTree (Version $ VersionNumber [Just 2, Just 1, Just 0]) []
                            ]
                        ]
                    ]
                ]
            ],
            RoseTree (Version $ VersionNumber [Nothing, Nothing, Just 3]) [
                RoseTree (Version $ VersionNumber [Just 3, Nothing, Nothing]) []
            ]
         ]
-- HELPER FUNCTIONS --

displayVersionTree t = putStrLn $ drawVerticalTree (versionTreeToStringTree t)
displayAllowedChanges t = putStrLn $ drawVerticalTree (versionTreeToAllowedChangesTree t)
