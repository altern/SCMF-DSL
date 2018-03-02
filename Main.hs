{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import RoseTree
import Version
import VersionNumber
import Repository
import Artifact
import ArtifactTree
import Platform
import FileOperation
import UserOperation
import MaturityLevel
import Document 

import Text.Regex.PCRE
import System.Console.Haskeline
import System.IO
import Control.Monad.State.Strict
import Data.List 
import Data.Maybe 

data RepositoryState = RepositoryState {
  repository :: Repository,
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
            ":show", ":init", ":save", ":load", 
            ":editBranch", ":showContent", 
            ":newSupportBranch", ":newReleaseBranch", ":newRevision", ":promoteSnapshot",
            ":newSupportSnapshot", ":newReleaseSnapshot", ":toggleRevisions", ":toggleMaturityLevels"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) wordList

mySettings :: Settings (StateT RepositoryState IO)
mySettings = Settings { historyFile = Just "myhist"
                      , complete = completeWord Nothing " \t" $ return . searchFunc
                      , autoAddHistory = True
                      }

help :: InputT (StateT RepositoryState IO)()
help = liftIO $ mapM_ putStrLn
       [ ""
       , ":help     - this help"
       , ":q        - quit"
       , ":commands - list available commands"
       , ""
       ]
       
commands :: InputT (StateT RepositoryState IO)()
commands = liftIO $ mapM_ putStrLn
       [ ""
       , ":show                 - display version tree "
       , ":init                 - init version tree (replace existing)"
       , ":save                 - save results to file"
       , ":load                 - load results from file"
       , ":editBranch           - edit tree element name and content" 
       , ":showContent          - show tree element name and content" 
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
       
showCommand :: InputT (StateT RepositoryState IO)()
showCommand = do
  RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get
  liftIO $ displayTree repository
{-  liftIO $ if displayRevisionsFlag -}
    {-then if displayMaturityLevelsFlag -}
      {-then displayTree $ fmap toMaturityVersion repository-}
      {-else displayTree $ fmap toVersion repository-}
    {-else if displayMaturityLevelsFlag -}
      {-then displayTree $ filterTree isRevision $ fmap toMaturityVersion repository-}
      {-else displayTree $ filterTree isRevision $ fmap toVersion repository-}
 
initCommand :: InputT (StateT RepositoryState IO) ()
initCommand = do
  RepositoryState _ displayRevisionsFlag displayMaturityLevelsFlag <-get
  put $ RepositoryState initialRepository displayRevisionsFlag displayMaturityLevelsFlag

newCommand :: (Version -> Repository -> Repository) -> String -> InputT (StateT RepositoryState IO) ()
newCommand newFunc message = do
  RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get
  versionInput <- getInputLine message
  case versionInput of
    Nothing -> put $ RepositoryState (newFunc initialVersion repository) displayRevisionsFlag displayMaturityLevelsFlag
    Just stringVersion -> put $ RepositoryState (newFunc (stringToVersion stringVersion) repository) displayRevisionsFlag displayMaturityLevelsFlag

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

parseInput :: String -> InputT (StateT RepositoryState IO)()
parseInput inp
  | inp =~ "^\\:q"        = return ()
                            
  | inp =~ "^\\:he"       = help >> mainLoop
                            
  | inp =~ "^\\:commands" = commands >> mainLoop
  

  | inp =~ "^\\:init" = do
    initCommand 
    showCommand
    mainLoop 

  | inp =~ "^\\:save" = do
    RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get 
    liftIO $ saveToFile repository
    mainLoop
    
  | inp =~ "^\\:load" = do
    RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryState loadRepositoryFromFile displayRevisionsFlag displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:editBranch" = do
    RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get
    branchVersionInput <- getInputLine "\tEnter version of the branch to edit: "
    case branchVersionInput of
      Nothing -> put $ RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag
      Just stringBranchVersion -> if (null stringBranchVersion) then do
        outputStrLn $ "Version cannot be empty. Aborting operation"
        put $ RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag
        else 
          let branchVersion = stringToVersion stringBranchVersion in 
          if (isRevision branchVersion) then do 
            outputStrLn $ "You should enter branch version. Aborting operation"
            put $ RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag
          else do 
            newContentInput <- getInputLineWithInitial "\tEnter new contents of the branch: " (getRepositoryContentByVersion branchVersion repository, "")
            case newContentInput of
              Nothing -> put $ RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag  
              Just newContent -> put $ RepositoryState (editBranch branchVersion newContent repository) displayRevisionsFlag displayMaturityLevelsFlag
          {-showContentsCommand-}
    mainLoop

  | inp =~ "^\\:showContent" = do
    RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get
    versionInput <- getInputLine "\tEnter version of the node to show: "
    case versionInput of 
      Nothing -> put $ RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag 
      Just stringVersion -> let 
        version = stringToVersion stringVersion 
        content = getRepositoryContentByVersion version repository 
        in ( do
          outputStrLn $ show content
          put $ RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag 
        )
    mainLoop

  | inp =~ "^\\:show" = showCommand >> mainLoop 

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
    RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryState repository (not displayRevisionsFlag) displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:toggleMaturityLevels" = do
    RepositoryState repository displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryState repository displayRevisionsFlag ( not displayMaturityLevelsFlag )
    showCommand
    mainLoop

  | inp =~ ":" = do
    outputStrLn $ "\nNo command \"" ++ inp ++ "\"\n"
    mainLoop
    
  | otherwise = handleInput inp

handleInput :: String -> InputT (StateT RepositoryState IO)()
handleInput inp = mainLoop

mainLoop :: InputT (StateT RepositoryState IO)()
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

main :: IO ((), RepositoryState)
main = do 
    greet 
    runStateT (runInputT mySettings mainLoop) $ RepositoryState initialRepository defaultDisplayRevisionsFlagValue defaultDisplayMaturityLevelsFlagValue
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
    
