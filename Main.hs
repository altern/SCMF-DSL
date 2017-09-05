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
import Data.List 

data VersionTreeState = VersionTreeState {
  versionTree :: VersionTree,
  displayRevisionsFlag :: Bool
}

defaultDisplayRevisionsFlagValue :: Bool
defaultDisplayRevisionsFlagValue = True

instance MonadState s m => MonadState s (InputT m) where
    get = lift get
    put = lift . put
    state = lift . state

wordList = [":help", ":q", ":commands", 
            ":show", ":save", ":load", 
            ":edit", ":newSupportBranch", ":newReleaseBranch", 
            ":newSupportSnapshot", ":newReleaseSnapshot", ":toggleShowRevisions"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) wordList

mySettings :: Settings (StateT VersionTreeState IO)
mySettings = Settings { historyFile = Just "myhist"
                      , complete = completeWord Nothing " \t" $ return . searchFunc
                      , autoAddHistory = True
                      }

help :: InputT (StateT VersionTreeState IO)()
help = liftIO $ mapM_ putStrLn
       [ ""
       , ":help     - this help"
       , ":q        - quit"
       , ":commands - list available commands"
       , ""
       ]
       
commands :: InputT (StateT VersionTreeState IO)()
commands = liftIO $ mapM_ putStrLn
       [ ""
       , ":show                 - display version tree "
       , ":save                 - save results to file"
       , ":load                 - load results from file"
       , ":edit                 - edit version tree"
       , ":newSupportBranch     - generate new support branch in version tree"
       , ":newReleaseBranch     - generate new release branch in version tree"
       , ":newSupportSnapshot   - generate new support snapshot in version tree"
       , ":newReleaseSnapshot   - generate new release snapshot in version tree"
       , ":toggleShowRevisions  - toggle display of the revisions in version tree"
       , ""
       ]
       
showCommand :: InputT (StateT VersionTreeState IO)()
showCommand = do
  VersionTreeState versionTree displayRevisionsFlag <- get
  liftIO $ if displayRevisionsFlag 
    then displayVersionTree versionTree
    else displayVersionTree $ filterTree isRevision versionTree


parseInput :: String -> InputT (StateT VersionTreeState IO)()
parseInput inp
  | inp =~ "^\\:q"        = return ()
                            
  | inp =~ "^\\:he"       = help >> mainLoop
                            
  | inp =~ "^\\:commands" = commands >> mainLoop
  
  | inp =~ "^\\:show" = showCommand >> mainLoop 

  | inp =~ "^\\:save" = do
    VersionTreeState versionTree displayRevisionsFlag <- get 
    liftIO $ saveToFile versionTree
    mainLoop
    
  | inp =~ "^\\:load" = do
    {-liftIO $ loadVersionTreeFromFile -}
    VersionTreeState versionTree displayRevisionsFlag <- get
    put $ VersionTreeState loadVersionTreeFromFile displayRevisionsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:newSupportBranch" = do
    VersionTreeState versionTree displayRevisionsFlag <- get
    versionInput <- getInputLine "\tEnter version, which will be used to append new support branch to: "
    searchVersion <- case versionInput of
      Nothing -> put $ VersionTreeState (newSupportBranch initialVersion versionTree) displayRevisionsFlag
      Just stringVersion -> put $ VersionTreeState (newSupportBranch (stringToVersion stringVersion) versionTree) displayRevisionsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:newReleaseBranch" = do
    VersionTreeState versionTree displayRevisionsFlag <- get
    versionInput <- getInputLine "\tEnter version, which will be used to append new release branch to: "
    searchVersion <- case versionInput of
      Nothing -> put $ VersionTreeState (newReleaseBranch initialVersion versionTree) displayRevisionsFlag
      Just stringVersion -> put $ VersionTreeState (newReleaseBranch (stringToVersion stringVersion) versionTree) displayRevisionsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:newSupportSnapshot" = do
    VersionTreeState versionTree displayRevisionsFlag <- get
    versionInput <- getInputLine "\tEnter version, which will be used to append new support snapshot to: "
    searchVersion <- case versionInput of
      Nothing -> put $ VersionTreeState (newSupportSnapshot initialVersion versionTree) displayRevisionsFlag
      Just stringVersion -> put $ VersionTreeState (newSupportSnapshot (stringToVersion stringVersion) versionTree) displayRevisionsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:newReleaseSnapshot" = do
    VersionTreeState versionTree displayRevisionsFlag <- get
    versionInput <- getInputLine "\tEnter version, which will be used to append new release snapshot to: "
    searchVersion <- case versionInput of
      Nothing -> put $ VersionTreeState (newReleaseSnapshot initialVersion versionTree) displayRevisionsFlag
      Just stringVersion -> put $ VersionTreeState (newReleaseSnapshot (stringToVersion stringVersion) versionTree) displayRevisionsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:toggleShowRevisions" = do
    VersionTreeState versionTree displayRevisionsFlag <- get
    put $ VersionTreeState versionTree (not displayRevisionsFlag)
    showCommand
    mainLoop

  | inp =~ ":" = do
    outputStrLn $ "\nNo command \"" ++ inp ++ "\"\n"
    mainLoop
    
  | otherwise = handleInput inp

handleInput :: String -> InputT (StateT VersionTreeState IO)()
handleInput inp = mainLoop

mainLoop :: InputT (StateT VersionTreeState IO)()
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

main :: IO ((), VersionTreeState)
main = do 
    greet 
    runStateT (runInputT mySettings mainLoop) $ VersionTreeState initialVersionTree defaultDisplayRevisionsFlagValue
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
    
