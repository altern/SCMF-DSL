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
import MaturityLevel

import Text.Regex.PCRE
import System.Console.Haskeline
import System.IO
import Control.Monad.State.Strict
import Data.List 
import Data.Maybe 

data VersionTreeState = VersionTreeState {
  versionTree :: VersionTree,
  displayRevisionsFlag :: Bool,
  displayMaturityLevelsFlag :: Bool
}

defaultDisplayRevisionsFlagValue :: Bool
defaultDisplayRevisionsFlagValue = True

defaultDisplayMaturityLevelsFlagValue :: Bool
defaultDisplayMaturityLevelsFlagValue = False

instance MonadState s m => MonadState s (InputT m) where
    get = lift get
    put = lift . put
    state = lift . state

wordList = [":help", ":q", ":commands", 
            ":show", ":save", ":load", 
            ":edit", ":newSupportBranch", ":newReleaseBranch", ":newRevision", ":promoteSnapshot",
            ":newSupportSnapshot", ":newReleaseSnapshot", ":toggleRevisions", ":toggleMaturityLevels"]

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
       , ":newRevision          - generate new revision in version tree"
       , ":promoteSnapshot      - generate new snapshot with incremented maturity level in version tree"
       , ":toggleRevisions      - toggle display of the revisions in version tree"
       , ":toggleMaturityLevels - toggle display of the maturity levels in version tree"
       , ""
       ]
       
showCommand :: InputT (StateT VersionTreeState IO)()
showCommand = do
  VersionTreeState versionTree displayRevisionsFlag displayMaturityLevelsFlag <- get
  liftIO $ if displayRevisionsFlag 
    then if displayMaturityLevelsFlag 
      then displayVersionTree $ fmap toMaturityVersion versionTree
      else displayVersionTree $ fmap toVersion versionTree
    else if displayMaturityLevelsFlag 
      then displayVersionTree $ filterTree isRevision $ fmap toMaturityVersion versionTree
      else displayVersionTree $ filterTree isRevision $ fmap toVersion versionTree

newCommand :: (Version -> VersionTree -> VersionTree) -> String -> InputT (StateT VersionTreeState IO) ()
newCommand newFunc message = do
  VersionTreeState versionTree displayRevisionsFlag displayMaturityLevelsFlag <- get
  versionInput <- getInputLine (message)
  case versionInput of
    Nothing -> put $ VersionTreeState (newFunc initialVersion versionTree) displayRevisionsFlag displayMaturityLevelsFlag
    Just stringVersion -> put $ VersionTreeState (newFunc (stringToVersion stringVersion) versionTree) displayRevisionsFlag displayMaturityLevelsFlag

{-newCommandMaturityLevel :: (Version -> VersionTree -> VersionTree) -> String -> InputT (StateT VersionTreeState IO) ()-}
{-newCommandMaturityLevel newFunc message = do-}
  {-VersionTreeState versionTree displayRevisionsFlag displayMaturityLevelsFlag <- get-}
  {-versionInput <- getInputLine ("\tEnter version, which will be used to append new " ++ message ++ " to: ")-}
  {-maturityLevelInput <- getInputLine "\tEnter maturity level for the new version: "-}
  {-let (maturityLevel) = case maturityLevelInput of-}
        {-Nothing -> Dev-}
        {-Just stringMaturityLevel -> read stringMaturityLevel -}
  {-case versionInput of-}
    {-Nothing -> put $ VersionTreeState (newFunc initialVersion maturityLevel versionTree) displayRevisionsFlag displayMaturityLevelsFlag-}
    {-Just stringVersion -> put $ VersionTreeState (newFunc (stringToVersion stringVersion) maturityLevel versionTree) displayRevisionsFlag displayMaturityLevelsFlag-}

parseInput :: String -> InputT (StateT VersionTreeState IO)()
parseInput inp
  | inp =~ "^\\:q"        = return ()
                            
  | inp =~ "^\\:he"       = help >> mainLoop
                            
  | inp =~ "^\\:commands" = commands >> mainLoop
  
  | inp =~ "^\\:show" = showCommand >> mainLoop 

  | inp =~ "^\\:save" = do
    VersionTreeState versionTree displayRevisionsFlag displayMaturityLevelsFlag <- get 
    liftIO $ saveToFile versionTree
    mainLoop
    
  | inp =~ "^\\:load" = do
    VersionTreeState versionTree displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ VersionTreeState loadVersionTreeFromFile displayRevisionsFlag displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:newSupportBranch" = do
    newCommand newSupportBranch "\tEnter version, which will be used to append new support branch to: "
    showCommand
    mainLoop

  | inp =~ "^\\:newReleaseBranch" = do
    newCommand newReleaseBranch "\tEnter version, which will be used to append new release branch to: "
    showCommand
    mainLoop

  | inp =~ "^\\:newSupportSnapshot" = do
    newCommand newSupportSnapshot "\tEnter version, which will be used to append new support snapshot to: "
    showCommand
    mainLoop

  | inp =~ "^\\:newReleaseSnapshot" = do
    newCommand newReleaseSnapshot "\tEnter version, which will be used to append new release snapshot to: "
    showCommand
    mainLoop

  | inp =~ "^\\:newRevision" = do
    newCommand newRevision "\tEnter version, which will be used to append new revision to: "
    showCommand
    mainLoop

  | inp =~ "^\\:promoteSnapshot " = do
    newCommand promoteSnapshot "\tEnter version of the snapshot you want to be promoted: "
    showCommand
    mainLoop

  | inp =~ "^\\:toggleRevisions" = do
    VersionTreeState versionTree displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ VersionTreeState versionTree (not displayRevisionsFlag) displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:toggleMaturityLevels" = do
    VersionTreeState versionTree displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ VersionTreeState versionTree displayRevisionsFlag ( not displayMaturityLevelsFlag )
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
    runStateT (runInputT mySettings mainLoop) $ VersionTreeState initialVersionTree defaultDisplayRevisionsFlagValue defaultDisplayMaturityLevelsFlagValue
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
    
