module SCMF where

import RoseTree
import Version
import VersionNumber
import VersionTree
import Artifact
import ArtifactTree
import Data.List.Split
import Data.List

main :: IO ()
main = do 
    let t = (generateSnapshot artifactTree3 ( searchArtifactTree artifactTree3 (Version NumberPlaceholder) ) )
    displayRepresentationsOfArtifactTree artifactTree3
    displayArtifactTree t
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
    