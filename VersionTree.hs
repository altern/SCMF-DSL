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
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Control.Applicative

type VersionTree = RoseTree Version
type VersionTreeList = [VersionTree]

instance JSON.ToJSON VersionTree where
	toJSON (RoseTree n cs) =
		JSON.object [T.pack "value" JSON..= JSON.toJSON n
                   , T.pack "children" JSON..= JSON.toJSON cs]


-- data Version = MaturityVersion MaturityLevel VersionNumber  -- Dev/1.x.0, Test/1.x.3, User/1.x.4, User/2.5.1, ...
             -- | Version VersionNumber

-- data VersionCompound = NumberPlaceholder                    
                     -- | Number Int                           
                     -- deriving (Show)

-- data VersionNumber = VersionCompound VersionCompound
       -- | VersionNumber VersionCompound VersionNumber
       -- deriving (Show)

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
versionTreeToStringTree (RoseTree num []) = Node (versionToString num) []
versionTreeToStringTree (RoseTree num (x:[])) = Node (versionToString num) [ versionTreeToStringTree x ]
versionTreeToStringTree (RoseTree num (x:xs)) = Node (versionToString num) ( (versionTreeToStringTree x) : (versionListToStringTreeList xs) )

-- TODO: implement toJSON and FromJSON for versionTrees
-- instance JSON.ToJSON VersionTree where
	-- toJSON (RoseTree version children) = 
		-- JSON.object [ T.pack "version" JSON..= (T.pack $ show version)]
		-- JSON.object [ T.pack "children" JSON..= (JSON.toJSON children)]

-- instance JSON.FromJSON VersionTree where
    -- parseJSON (JSON.Object vTree) = liftM stringToVersion ( vTree JSON..: T.pack "versionTree" )
    -- parseJSON _ = mzero

-- VERSION TREE CONVERSION TO ALLOWED CHANGES --

-- versionListToAllowedChangesList :: VersionTreeList -> StringTreeList
-- versionListToAllowedChangesList [] = []
-- versionListToAllowedChangesList(x:[]) = [versionTreeToAllowedChangesTree x]
-- versionListToAllowedChangesList (x:xs) = (versionTreeToAllowedChangesTree x):(versionListToAllowedChangesList xs) 

-- versionTreeToAllowedChangesTree :: VersionTree -> StringTree
-- versionTreeToAllowedChangesTree (RoseTree num []) = Node (allowedChangesToString ( detectAllowedChanges num)) []
-- versionTreeToAllowedChangesTree (RoseTree num (x:[])) = Node (allowedChangesToString ( detectAllowedChanges num)) [ versionTreeToAllowedChangesTree x ]
-- versionTreeToAllowedChangesTree (RoseTree num (x:xs)) = Node (allowedChangesToString ( detectAllowedChanges num)) ( (versionTreeToAllowedChangesTree x) : (versionListToAllowedChangesList xs) )

class VersionTreeAppend structure where
    versionTreeAppend :: structure -> structure

instance VersionTreeAppend VersionTreeList where 
    versionTreeAppend [] = [RoseTree (Version (VersionCompound (Number 0))) []]
    versionTreeAppend v@((RoseTree (Version (VersionCompound (Number n))) []):[]) = v ++ [(RoseTree (Version (VersionCompound (Number (n + 1)))) [] )]
    versionTreeAppend v@((RoseTree (Version (VersionCompound NumberPlaceholder)) []):[]) = v
    versionTreeAppend ((RoseTree (Version (VersionCompound NumberPlaceholder)) x):[]) = ((RoseTree (Version (VersionCompound NumberPlaceholder)) (versionTreeAppend x)):[])
    versionTreeAppend (x:xs) = [x] ++ (versionTreeAppend xs)

instance VersionTreeAppend VersionTree where 
    versionTreeAppend (RoseTree (Version (VersionCompound NumberPlaceholder)) list) = (RoseTree (Version (VersionCompound NumberPlaceholder)) (versionTreeAppend list))
    versionTreeAppend a@(RoseTree (Version (VersionCompound (Number n))) list) = a

class FindLatestVersion a where 
    findLatestVersion :: a -> Version

instance FindLatestVersion VersionTree where    
    findLatestVersion ( RoseTree version [] ) = version
    findLatestVersion ( RoseTree version1 list ) = case ( version1 > ( findLatestVersion list ) ) of
        True -> version1
        False -> findLatestVersion list

instance FindLatestVersion VersionTreeList where
    findLatestVersion [] = initialVersion (Number 0)
    findLatestVersion (x:[]) = findLatestVersion x
    findLatestVersion (x:xs) = case ((findLatestVersion x) > (findLatestVersion xs)) of
        True -> (findLatestVersion x)
        False -> (findLatestVersion xs)

class FindParentVersion a where 
    findParentVersion :: a -> Version -> Version

instance FindParentVersion VersionTree where
    findParentVersion ( RoseTree _ [] ) _ = initialVersion (Number 0)
    findParentVersion ( RoseTree parentVersion list ) version = case (searchVersionTreeChildren list version) of
        True -> parentVersion
        False -> findParentVersion list version
        
instance FindParentVersion VersionTreeList where
    findParentVersion [] _ = initialVersion (Number 0)
    findParentVersion (x:xs) version = case ( isInitialVersion (findParentVersion x version) ) of
        True -> findParentVersion xs version
        False -> findParentVersion x version

class SearchVersionTreeChildren a where
    searchVersionTreeChildren :: VersionTreeList -> a -> Bool    
    
instance SearchVersionTreeChildren Version where
    searchVersionTreeChildren [] _ = False
    searchVersionTreeChildren ((RoseTree version1 _):xs) version2 = (version1 == version2) || (searchVersionTreeChildren xs version2)

appendNewVersion :: VersionTree -> Version -> VersionTree 
appendNewVersion vTree version = treeInsert vTree version ( generateNewVersion version ) 

-- EXAMPLES --

vTree1 :: VersionTree
vTree1 = RoseTree (Version (VersionCompound NumberPlaceholder)) []

strTree1 :: StringTree
strTree1 = Node "X" []

vTree2 :: VersionTree
vTree2 = RoseTree (Version (VersionCompound NumberPlaceholder)) [RoseTree (Version (VersionCompound (Number 1)) ) []]

strTree2 :: StringTree
strTree2 = Node "X" [Node "1" []]

vTree3 :: VersionTree
vTree3 = RoseTree (Version (VersionCompound NumberPlaceholder ) )  [RoseTree (Version ( VersionCompound (Number 1))) [], RoseTree ((Version (VersionCompound (Number 2)))) []]

strTree3 :: StringTree
strTree3 = Node "X" [Node "1" [], Node "2" []]

vTree4 :: VersionTree
vTree4 = RoseTree (Version (VersionCompound NumberPlaceholder)) [RoseTree (Version (VersionCompound (Number 1))) [], RoseTree ((Version (VersionCompound (Number 2)))) [], RoseTree ((Version (VersionCompound (Number 3)))) [], RoseTree ((Version (VersionCompound (Number 4)))) []]

-- HELPER FUNCTIONS --

displayVersionTree t = putStrLn $ drawVerticalTree (versionTreeToStringTree t)
-- displayAllowedChanges t = putStrLn $ drawVerticalTree (versionTreeToAllowedChangesTree t)