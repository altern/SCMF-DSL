module SCMF where

import RoseTree
import Version
import VersionNumber
import VersionTree
import Artifact
import ArtifactTree
import Data.List.Split
import Data.List
import Platform
import FileOperation
import UserOperation

import Control.Monad
import Control.Monad.IO.Class
import Text.Regex.PCRE
import System.Console.Haskeline

help :: InputT IO ()
help = liftIO $ mapM_ putStrLn
       [ ""
       , ":help     - this help"
       , ":q        - quit"
       , ":commands - list available commands"
       , ""
       ]
       
commands :: InputT IO ()
commands = liftIO $ mapM_ putStrLn
       [ ""
       , ":show     - display artifact trees, version trees, artifacts, etc. "
       , ":save     - save results to file"
       , ":edit     - edit artifact tree"
       , ":search   - search for specific element in artifact tree"
       , ""
       ]
       
-- showCommand :: InputT IO ()
-- showCommand inp = putStrLn $ action Show t :: IO ()

parseInput :: String -> InputT IO ()
parseInput inp
  | inp =~ "^\\:q"        = return ()
                            
  | inp =~ "^\\:he"       = help >> mainLoop
                            
  | inp =~ "^\\:commands" = commands >> mainLoop
  
  | inp =~ "^\\:show" = do
    -- outputStrLn $ action Show t :: IO ()
    mainLoop 
                                                   
  | inp =~ ":" = do
    outputStrLn $ "\nNo command \"" ++ inp ++ "\"\n"
    mainLoop
    
  | otherwise = handleInput inp

handleInput :: String -> InputT IO ()
handleInput inp = mainLoop

mainLoop :: InputT IO ()
mainLoop = do
  inp <- getInputLine "% "
  maybe (return ()) (parseInput) inp

greet :: IO ()
greet = mapM_ putStrLn
        [ ""
        , "          SCMF-DSL"
        , "=============================="
        , "For help type \":help\""
        , ""
        ]

main :: IO ()
main = do 
    greet 
    runInputT defaultSettings (mainLoop)
    
    -- tree <- loadArtifactTreeFromFile
    -- let t = (generateSnapshot artifactTree3 ( searchArtifactTree artifactTree3 (Version NumberPlaceholder) ) )
    -- let db = (deploy t deploymentRules platformDB )
    -- displayRepresentationsOfArtifactTree t
    -- displayPlatformsForArtifactTree t
    -- putStrLn $ show deploymentRules
    -- putStrLn ""
    -- putStrLn $ show db
    -- putStrLn ""
    -- -- displayArtifactTree t
    -- print ("Searching artifact tree for version " ++ (show v1) ++ "... Result: ")
    -- print (searchArtifactTree artifactTree3 v1)
    -- print ("")
    -- print ("Searching artifact tree for version " ++ (show v2) ++ "... Result: ")
    -- print (searchArtifactTree artifactTree3 v2)
    -- print ("")
    -- print ("Searching artifact tree for version " ++ (show v3) ++ "... Result: ")
    -- print (searchArtifactTree artifactTree3 v3)
    -- print ("")
    -- print ("Searching artifact tree for artifact " ++ (show artifact1) ++ "... Result: ")
    -- print (searchArtifactTree artifactTree3 artifact1)
    -- print ("")
    -- print ("Searching artifact tree for artifact " ++ (show artifact2) ++ "... Result: ")
    -- print (searchArtifactTree artifactTree3 artifact2)
    -- print ("")
    -- print ("Searching artifact tree for artifact " ++ (show artifact3) ++ "... Result: ")
    -- print (searchArtifactTree artifactTree3 artifact3)
--  print $ parseOnly stringToVersionNumber "1"
    