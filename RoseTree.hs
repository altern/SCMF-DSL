{-# LANGUAGE FlexibleInstances, OverloadedStrings, DeriveFunctor #-}
module RoseTree where

import Data.Tree
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Aeson.Types as AT
import Control.Applicative
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString.Char8

data RoseTree a = Empty | RoseTree a [RoseTree a]
                   deriving (Show, Functor)

type RoseTreeList a = [RoseTree a]

type StringTree = Tree String
type StringTreeList = [StringTree]


-- data RoseTree2 = RoseNode Int [RoseTree2] deriving (Show)

instance (Show a) => JSON.ToJSON (RoseTree a) where
    toJSON (RoseTree n cs) =
        JSON.object [T.pack "value" JSON..= show n
        , T.pack "children" JSON..= JSON.toJSON cs] 

instance (Show a, JSON.FromJSON a) => JSON.FromJSON (RoseTree a) where
    parseJSON (JSON.Object o) =
        RoseTree <$> o JSON..: T.pack "value"
        <*> o JSON..: T.pack "children"
        
parseRoseTreeFromJSON :: (Show a, JSON.FromJSON a) => String -> (RoseTree a)
parseRoseTreeFromJSON json = 
      let bs = BS.pack json in case parse JSON.json bs of
               (Done rest r) -> case AT.parseMaybe JSON.parseJSON r of
                    (Just x) -> x
                    Nothing -> Empty 
               _ -> Empty

-- instance (JSON.ToJSON v) => JSON.ToJSON (RoseTree v) where
    -- toJSON (RoseTree root branches) = JSON.toJSON (root, branches)

-- instance JSON.ToJSON (RoseTree v) where
-- toJSON (RoseTree root children) = 
    -- JSON.object [T.pack "value" JSON..= show root
               -- , T.pack "children" JSON..= JSON.toJSON children]

-- instance JSON.ToJSON RoseTreeList where
-- toJSON (x:xs) = JSON.toJSON (x, xs)
-- toJSON (x:[]) = JSON.toJSON (x)
-- toJSON [] = []

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
    
extractChild :: RoseTree a -> RoseTree a
extractChild (RoseTree _ (x:[])) = x
extractChild rTree = rTree

filterTree :: ( a -> Bool ) -> RoseTree a -> RoseTree a
filterTree condition rTree@(RoseTree a list) = 
    if (condition $ a) 
      then filterTree condition (extractChild rTree)  
      else (RoseTree a (filterTreeList condition list)) 

filterTreeList :: (a -> Bool) -> [RoseTree a] -> [RoseTree a]
filterTreeList condition [] = []
filterTreeList condition (x:xs) = [filterTree condition x] ++ (filterTreeList condition xs)

