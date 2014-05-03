module RoseTree where

import Data.Tree

data RoseTree a = RoseTree a [RoseTree a]
                   deriving (Show)
                   
type StringTree = Tree String
type StringTreeList = [StringTree]

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

-- HELPER FUNCTIONS

-- data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)
-- type IntTree = Tree Integer

-- fl :: Tree a -> [a]
-- fl (Node n st) = n : concat (map fl st)
-- fl (Node n st) = n : concat [fl t | t <- st]

-- leaf :: a -> Tree a
-- leaf x = Node x Empty Empty

-- insert :: (Ord a) => Tree a -> a -> Tree a
-- insert Empty x = leaf x
-- insert (Node x l r) y = case compare y x of
--  GT -> Node x l (insert r y)
--  _  -> Node x (insert l y) r

-- intTree :: IntTree
-- intTree = Node 0 ( Node 1 Empty Empty ) ( Node 2 Empty Empty )