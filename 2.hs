module SCMF

where

import Data.List.Split
import Data.List
import Data.Tree
import Data.Tree.Pretty

data VersionNumber = NumberPlaceholder                      -- X
                     | Number Int                           -- 1, 2, 3, ..., 45, ... 
                     deriving (Show)
 
data AllowedChanges = Any
                    | None
                    deriving (Show)
                    
detectAllowedChanges :: VersionNumber -> AllowedChanges
detectAllowedChanges NumberPlaceholder  = Any
detectAllowedChanges (Number n)         = None 
 
data RoseTree a = RoseTree a [RoseTree a]
                   deriving (Show)

type VersionTree = RoseTree VersionNumber
type StringTree = Tree String
type VersionTreeList = [VersionTree]
type StringTreeList = [StringTree]

versionNumberToString :: VersionNumber -> String
versionNumberToString (Number n) = (show n)
versionNumberToString NumberPlaceholder = "X"

allowedChangesToString :: AllowedChanges -> String
allowedChangesToString None = "None"
allowedChangesToString Any = "Any"

versionListToStringTreeList :: VersionTreeList -> StringTreeList
versionListToStringTreeList [] = []
versionListToStringTreeList (x:[]) = [versionTreeToStringTree x]
versionListToStringTreeList (x:xs) = (versionTreeToStringTree x):(versionListToStringTreeList xs) 

versionTreeToStringTree :: VersionTree -> StringTree
versionTreeToStringTree (RoseTree num []) = Node (versionNumberToString num) []
versionTreeToStringTree (RoseTree num (x:[])) = Node (versionNumberToString num) [ versionTreeToStringTree x ]
versionTreeToStringTree (RoseTree num (x:xs)) = Node (versionNumberToString num) ( (versionTreeToStringTree x) : (versionListToStringTreeList xs) )

versionListToAllowedChangesList :: VersionTreeList -> StringTreeList
versionListToAllowedChangesList [] = []
versionListToAllowedChangesList(x:[]) = [versionTreeToAllowedChangesTree x]
versionListToAllowedChangesList (x:xs) = (versionTreeToAllowedChangesTree x):(versionListToAllowedChangesList xs) 

versionTreeToAllowedChangesTree :: VersionTree -> StringTree
versionTreeToAllowedChangesTree (RoseTree num []) = Node (allowedChangesToString ( detectAllowedChanges num)) []
versionTreeToAllowedChangesTree (RoseTree num (x:[])) = Node (allowedChangesToString ( detectAllowedChanges num)) [ versionTreeToAllowedChangesTree x ]
versionTreeToAllowedChangesTree (RoseTree num (x:xs)) = Node (allowedChangesToString ( detectAllowedChanges num)) ( (versionTreeToAllowedChangesTree x) : (versionListToAllowedChangesList xs) )

vTree1 :: VersionTree
vTree1 = RoseTree NumberPlaceholder []

strTree1 :: StringTree
strTree1 = Node "X" []

vTree2 :: VersionTree
vTree2 = RoseTree NumberPlaceholder [RoseTree (Number 1) []]

strTree2 :: StringTree
strTree2 = Node "X" [Node "1" []]

vTree3 :: VersionTree
vTree3 = RoseTree NumberPlaceholder [RoseTree (Number 1) [], RoseTree (Number 2) []]

strTree3 :: StringTree
strTree3 = Node "X" [Node "1" [], Node "2" []]

vTree4 :: VersionTree
vTree4 = RoseTree NumberPlaceholder [RoseTree (Number 1) [], RoseTree (Number 2) [], RoseTree (Number 3) [], RoseTree (Number 4) []]

displayVersionTree t = putStrLn $ drawVerticalTree (versionTreeToStringTree t)
displayAllowedChanges t = putStrLn $ drawVerticalTree (versionTreeToAllowedChangesTree t)

data ChangeType = Add
                | Delete
                | Update

type Change = DocumentContent -> DocumentContent

appendLine :: String -> Change
appendLine line documentContent = documentContent ++ "\n" ++ line

deleteNth :: Integer -> [a] -> [a]
deleteNth n = foldr step [] . zip [1..]
    where step (i,x) acc = if (i `mod` n) == 0 then acc else x:acc

deleteLine :: Integer -> Change
deleteLine lineNumber documentContent = intercalate "\n" (deleteNth lineNumber (splitOn "\n" documentContent))


data DocumentOperation = Edit DocumentContent
                       | Release 
                       | Branch

type DocumentName = String
type DocumentContent = String
type Timestamp = Integer

type DocumentInfo = (DocumentName, Timestamp, VersionNumber, VersionTree)

data Document = Document DocumentInfo DocumentContent 
                deriving (Show)
                
generateDocument :: Document -> DocumentOperation -> Document
-- sem v@(DocumentVersion _ (Number _)) (Edit _) = v
-- sem (DocumentVersion (Doc name content) NumberPlaceholder) (Edit f) = DocumentVersion (Doc name (f content)) NumberPlaceholder
generateDocument v@(Document (name, timestamp, (Number n), history) content ) (Edit newContent) = error "not allowed to change immutable document"


detectChangeType :: Document -> Document -> ChangeType
-- sem ... added lines ... = Add
-- sem ... removed lines ... = Delete
-- sem ... changed lines ... = Edit
detectChangeType = undefined


-- Takes current version of the document, document content and produces new version for a document
produceNewVersion :: Document -> Document -> VersionNumber
produceNewVersion = undefined

            