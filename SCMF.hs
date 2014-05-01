module SCMF where

import Version
import VersionTree
import Artifact
import ArtifactTree
import Data.List.Split
import Data.List

-- appendLine :: String -> Change
-- appendLine line documentContent = documentContent ++ "\n" ++ line

-- deleteNth :: Integer -> [a] -> [a]
-- deleteNth n = foldr step [] . zip [1..]
    --where step (i,x) acc = if (i `mod` n) == 0 then acc else x:acc

-- deleteLine :: Integer -> Change
-- deleteLine lineNumber documentContent = intercalate "\n" (deleteNth lineNumber (splitOn "\n" documentContent))

-- Takes current version of the document, document content and produces new version for a document
-- produceNewVersion :: Document -> Document -> VersionNumber
-- produceNewVersion = undefined

main :: IO ()
main = do 
    displayRepresentationsOfArtifactTree artifactTree3
    print ("Searching artifact tree for version " ++ (show v1) ++ "... Result: ")
    print (searchArtifactTree artifactTree3 v1)
    print ("")
    print ("Searching artifact tree for version " ++ (show v2) ++ "... Result: ")
    print (searchArtifactTree artifactTree3 v2)
    print ("")
    print ("Searching artifact tree for version " ++ (show v3) ++ "... Result: ")
    print (searchArtifactTree artifactTree3 v3)
    print ("")
    print ("Searching artifact tree for artifact " ++ (show artifact1) ++ "... Result: ")
    print (searchArtifactTree artifactTree3 artifact1)
    print ("")
    print ("Searching artifact tree for artifact " ++ (show artifact2) ++ "... Result: ")
    print (searchArtifactTree artifactTree3 artifact2)
    print ("")
    print ("Searching artifact tree for artifact " ++ (show artifact3) ++ "... Result: ")
    print (searchArtifactTree artifactTree3 artifact3)