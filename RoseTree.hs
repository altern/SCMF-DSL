module RoseTree where

import Data.Tree
import qualified Data.Aeson as JSON

data RoseTree a = RoseTree a [RoseTree a]
                   deriving (Show)
                   
type StringTree = Tree String
type StringTreeList = [StringTree]

instance (JSON.ToJSON v) => JSON.ToJSON (RoseTree v) where
    toJSON (RoseTree root branches) = JSON.toJSON (root, branches)

instance (Eq a) => Eq (RoseTree a) where
    (RoseTree nodeA []) == (RoseTree nodeB [])              = nodeA == nodeB 
    (RoseTree nodeA (xA:[]) ) == (RoseTree nodeB (xB:[]) )    = (nodeA == nodeB) && (xA == xB)
    (RoseTree nodeA (xA:xsA) ) == (RoseTree nodeB (xB:xsB) )    = (nodeA == nodeB) && (xA == xB) && (xsA == xsB)
    _ == _                      = False

listContains :: Eq a => [RoseTree a] -> a -> Bool
listContains [] _ = False
listContains (x:xs) a = (treeContains x a) || (listContains xs a)

treeContains :: Eq a => RoseTree a -> a -> Bool
treeContains (RoseTree n1 (x:xs)) b = ( n1 == b ) || (listContains xs b) || (treeContains x b)
treeContains (RoseTree n1 []) b = (n1 == b)

listInsert :: (Ord a) => [a] -> a -> [a]
listInsert [] b = [b]
listInsert (x:xs) b 
    | x == b = (x:xs)
    | x < b = (x:(listInsert xs b))
    | x > b = (b:x:xs) 

treeInsert :: (Eq a) => RoseTree a -> a -> a -> RoseTree a
treeInsert (RoseTree a []) find insert
    | (a == find) = (RoseTree a [RoseTree insert []])
    | otherwise = (RoseTree a [])
treeInsert aTree@(RoseTree a l@(x:xs)) find insert 
    | (a == find) = (RoseTree a ( l ++ [RoseTree insert []] ) )
    | (treeContains x find) = (RoseTree a ( [treeInsert x find insert] ++ xs ) )
    | (listContains xs find) = (RoseTree a ( [x] ++ (treeListInsert xs find insert ) ) )
    | otherwise = aTree

treeListInsert :: (Eq a) => [RoseTree a] -> a -> a -> [RoseTree a]
treeListInsert [] _ insert = [ RoseTree insert [] ]
treeListInsert (x:xs) find insert
    | ( treeContains x find ) = [treeInsert x find insert] ++ xs
    | ( listContains xs find ) = [x] ++ ( treeListInsert xs find insert )

treeAppend :: (Ord a) => RoseTree a -> a -> RoseTree a
treeAppend (RoseTree a []) b = RoseTree a [ RoseTree b [] ]
treeAppend (RoseTree a list) b = (RoseTree a (list ++ [ RoseTree b [] ] ))

treeUpdate :: (Eq a) => RoseTree a -> a -> a -> RoseTree a
treeUpdate (RoseTree a []) find replace 
    | a == find = (RoseTree replace [])
    | otherwise = (RoseTree a [])
treeUpdate (RoseTree a list) find replace 
    | a == find = (RoseTree replace (treeListUpdate list find replace) )
    | otherwise = (RoseTree a (treeListUpdate list find replace) )

treeListUpdate :: (Eq a) => [RoseTree a] -> a -> a -> [RoseTree a]
treeListUpdate [] _ _ = []
treeListUpdate (x:xs) find replace
    | ( treeContains x find ) = [treeUpdate x find replace] ++ xs
    | ( listContains xs find ) = [x] ++ ( treeListUpdate xs find replace )
    | otherwise = (x:xs)
    