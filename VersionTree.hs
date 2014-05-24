{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module VersionTree where

import Data.Tree
import Data.Tree.Pretty

import RoseTree
import VersionNumber
import Version
import Artifact

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

-- VERSION TREE CONVERSION TO STRING --

versionListToStringTreeList :: VersionTreeList -> StringTreeList
versionListToStringTreeList [] = []
versionListToStringTreeList (x:[]) = [versionTreeToStringTree x]
versionListToStringTreeList (x:xs) = (versionTreeToStringTree x):(versionListToStringTreeList xs) 

versionTreeToStringTree :: VersionTree -> StringTree
versionTreeToStringTree (RoseTree num []) = Node (versionToString num) []
versionTreeToStringTree (RoseTree num (x:[])) = Node (versionToString num) [ versionTreeToStringTree x ]
versionTreeToStringTree (RoseTree num (x:xs)) = Node (versionToString num) ( (versionTreeToStringTree x) : (versionListToStringTreeList xs) )

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

instance VersionTreeAppend VersionTreeList where 
    versionTreeAppend [] = [RoseTree (Version (Number 0)) []]
    versionTreeAppend v@((RoseTree (Version (Number n)) []):[]) = v ++ [(RoseTree (Version (Number (n + 1))) [] )]
    versionTreeAppend v@((RoseTree (Version NumberPlaceholder) []):[]) = v
    versionTreeAppend ((RoseTree (Version NumberPlaceholder) x):[]) = ((RoseTree (Version NumberPlaceholder) (versionTreeAppend x)):[])
    versionTreeAppend (x:xs) = [x] ++ (versionTreeAppend xs)

instance VersionTreeAppend VersionTree where 
    versionTreeAppend (RoseTree (Version NumberPlaceholder) list) = (RoseTree (Version NumberPlaceholder) (versionTreeAppend list))
    versionTreeAppend a@(RoseTree (Version (Number n)) list) = a

-- versionTreeInsert :: (Ord a) => VersionTree -> Version -> VersionTree
-- versionTreeInsert RoseTree NumberPlaceholder [] = 
-- treeInsert (RoseTree elem (x:xs)) find insert
    -- | x == find = ( RoseTree elem ( [x] ++ [ RoseTree insert [] ] ++ xs ) )
    -- | x == find = ( RoseTree elem [] )
--    | (treeContains x elem) = treeInsert x find insert
--    | (listContains xs elem) = (RoseTree elem (listInsert xs (RoseTree insert [])))
--    | otherwise = error "Cannot find node to append after"

-- EXAMPLES --

vTree1 :: VersionTree
vTree1 = RoseTree (Version (NumberPlaceholder)) []

strTree1 :: StringTree
strTree1 = Node "X" []

vTree2 :: VersionTree
vTree2 = RoseTree (Version NumberPlaceholder) [RoseTree (Version (Number 1)) []]

strTree2 :: StringTree
strTree2 = Node "X" [Node "1" []]

vTree3 :: VersionTree
vTree3 = RoseTree (Version NumberPlaceholder) [RoseTree (Version (Number 1)) [], RoseTree ((Version (Number 2))) []]

strTree3 :: StringTree
strTree3 = Node "X" [Node "1" [], Node "2" []]

vTree4 :: VersionTree
vTree4 = RoseTree (Version NumberPlaceholder) [RoseTree (Version (Number 1)) [], RoseTree ((Version (Number 2))) [], RoseTree ((Version (Number 3))) [], RoseTree ((Version (Number 4))) []]

-- HELPER FUNCTIONS --

displayVersionTree t = putStrLn $ drawVerticalTree (versionTreeToStringTree t)
displayAllowedChanges t = putStrLn $ drawVerticalTree (versionTreeToAllowedChangesTree t)