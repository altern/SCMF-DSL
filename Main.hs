{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import RoseTree
import Version
import VersionNumber
import VersionTree
import Artifact
import ArtifactTree
import Platform
import FileOperation
import UserOperation

import Text.Regex.PCRE
import System.Console.Haskeline
import System.IO
import Control.Monad.State.Strict


versionTree :: VersionTree
versionTree = initialVersionTree

instance MonadState s m => MonadState s (InputT m) where
    get = lift get
    put = lift . put
    state = lift . state

help :: InputT (StateT VersionTree IO)()
help = liftIO $ mapM_ putStrLn
       [ ""
       , ":help     - this help"
       , ":q        - quit"
       , ":commands - list available commands"
       , ""
       ]
       
commands :: InputT (StateT VersionTree IO)()
commands = liftIO $ mapM_ putStrLn
       [ ""
       , ":show     - display version tree "
       , ":save     - save results to file"
       , ":edit     - edit version tree"
       , ":search   - search for specific element in version tree"
       , ":new      - generate new element in version tree"
       , ""
       ]
       
-- showCommand :: InputT (StateT VersionTree IO)()
-- showCommand inp = putStrLn $ action Show t :: IO ()

parseInput :: String -> InputT (StateT VersionTree IO)()
parseInput inp
  | inp =~ "^\\:q"        = return ()
                            
  | inp =~ "^\\:he"       = help >> mainLoop
                            
  | inp =~ "^\\:commands" = commands >> mainLoop
  
  | inp =~ "^\\:show" = do
    -- outputStrLn $ action Show t :: IO ()
    -- tree <- loadArtifactTreeFromFile
    -- displayRepresentationsOfArtifactTree tree
    versionTree <- get
    liftIO $ displayVersionTree versionTree
    mainLoop 

  | inp =~ "^\\:save" = do
    versionTree <- get 
    liftIO $ saveToFile versionTree
    mainLoop
    
  | inp =~ "^\\:load" = do
    {-liftIO $ loadVersionTreeFromFile -}
    versionTree <- get
    put loadVersionTreeFromFile
    mainLoop

  | inp =~ "^\\:new" = do
    versionTree <- get
    put $ newSupportBranch initialVersion versionTree
    mainLoop

  | inp =~ ":" = do
    outputStrLn $ "\nNo command \"" ++ inp ++ "\"\n"
    mainLoop
    
  | otherwise = handleInput inp

handleInput :: String -> InputT (StateT VersionTree IO)()
handleInput inp = mainLoop

mainLoop :: InputT (StateT VersionTree IO)()
mainLoop = do
  inp <- getInputLine "% "
  maybe (return ()) parseInput inp

greet :: IO ()
greet = mapM_ putStrLn
        [ ""
        , "          SCMF-DSL"
        , "=============================="
        , "For help type \":help\""
        , ""
        ]

main :: IO ((), VersionTree)
main = do 
    greet 
    runStateT (runInputT defaultSettings mainLoop) initialVersionTree  
    -- tree <- loadArtifactTreeFromFile
    -- let t = (generateSnapshot artifactTree3 ( searchArtifactTree artifactTree3 (Version NumberPlaceholder) ) )
    -- let db = (deploy t deploymentRules platformDB )
    -- displayRepresentationsOfArtifactTree tree
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
    
