{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import RoseTree
import Version
import VersionNumber
import Repository
import RepositoryMap
import Artifact
import ArtifactTree
import Platform
import FileOperation
import UserOperation
import MaturityLevel
import Document 
import Util

import Text.Regex.PCRE
import System.Console.Haskeline
import System.IO
import Control.Monad.State.Strict
import Data.List 
import qualified Data.Map as M
import Data.Maybe 

data RepositoryMapState = RepositoryMapState {
  repositoryMap :: RepositoryMap,
  selectedRepository :: String,
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

repositoryMapCommands = M.fromList [
            (":help","displays this help"), 
            (":q","quits the application"),
            (":commands","displays the list of available commands"), 
            (":show","shows the list of repositories with their version trees"), 
            (":init","initializes repository map from scratch"),
            (":save","saves repository map to a file"),
            (":load","loads repository map from a file"),
            (":edit","enters edit mode for specific repository selected by its name"), 
            (":rename","renames repository"), 
            (":new","adds new empty repository to the list"), 
            (":toggleRevisions","turns on/off display of revisions for version trees"), 
            (":toggleMaturityLevels","turns on/off display of maturity levels for version trees")
          ]
repositoryCommands = M.fromList [
            (":help","displays this help"), 
            (":q","quits repository editing mode"), 
            (":commands","displays the list of available commands"), 
            (":show", "displays version tree of the currently selected repository"),
            (":editBranch","edits contents of the specific branch"), 
            (":showContent","shows contents of the specific branch"), 
            (":newSupportBranch","adds new support branch to the version tree"), 
            (":newReleaseBranch","adds new release branch to the version tree"), 
            (":newRevision","adds new revision to the version tree"), 
            (":promoteSnapshot","promotes specific snapshot to the next maturity level"),
            (":newSupportSnapshot","adds new support snapshot to the version tree"), 
            (":newReleaseSnapshot","adds new release snapshot to the version tree"),
            (":reSnapshot","recreates snapshot with the same maturity level")
          ]

searchFunc :: RepositoryMapState -> String -> [Completion]
searchFunc (RepositoryMapState repositoryMap selectedRepository _ _) str = map simpleCompletion 
      $ filter (str `isPrefixOf`)
      $ if null selectedRepository 
          then M.keys repositoryMapCommands ++ M.keys repositoryMap
          else M.keys repositoryCommands

mySettings :: Settings (StateT RepositoryMapState IO)
mySettings = Settings { historyFile = Just "hist"
                      , complete = completeWord Nothing "\t" $ \str -> do 
                          data_ <- get
                          return $ searchFunc data_ str
                      , autoAddHistory = True
                      }

help :: InputT (StateT RepositoryMapState IO)()
help = commands
       
commands :: InputT (StateT RepositoryMapState IO)()
commands = do 
        (RepositoryMapState _ selectedRepository _ _ ) <- get
        liftIO $ mapM_ putStrLn $ if null selectedRepository 
            then M.elems $ M.mapWithKey (\k v -> k ++ "\t - " ++ v) repositoryMapCommands
            else M.elems $ M.mapWithKey (\k v -> k ++ "\t - " ++ v) repositoryCommands
       
showCommand :: InputT (StateT RepositoryMapState IO)()
showCommand = do
  RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
  if null selectedRepository 
    then liftIO $ displayRepositoryMap repositoryMap displayRevisionsFlag displayMaturityLevelsFlag 
    else liftIO $ let repository = (fromJust $ M.lookup selectedRepository repositoryMap) in
      do putStrLn $ show selectedRepository ++ " => "
         displayRepository repository displayRevisionsFlag displayMaturityLevelsFlag

initCommand :: InputT (StateT RepositoryMapState IO) ()
initCommand = do
  RepositoryMapState _ selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <-get
  put $ RepositoryMapState initialRepositoryMap "" displayRevisionsFlag displayMaturityLevelsFlag

newCommand :: (Version -> Repository -> Repository) -> String -> InputT (StateT RepositoryMapState IO) ()
newCommand newFunc message = do
  RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
  versionInput <- getInputLine message
  case versionInput of
    Nothing -> put $ RepositoryMapState 
      ( M.insert selectedRepository 
                 (newFunc initialVersion (fromJust $ M.lookup selectedRepository repositoryMap )) 
                 repositoryMap)
      selectedRepository 
      displayRevisionsFlag 
      displayMaturityLevelsFlag
    Just stringVersion -> put $ RepositoryMapState 
      ( M.insert selectedRepository 
                 (newFunc (stringToVersion stringVersion) (fromJust $ M.lookup selectedRepository repositoryMap ))
                 repositoryMap) 
      selectedRepository 
      displayRevisionsFlag 
      displayMaturityLevelsFlag

parseInput :: String -> InputT (StateT RepositoryMapState IO)()
parseInput inp
  | inp =~ "^\\:q" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    if null selectedRepository 
      then return () 
      else do 
        put $ RepositoryMapState repositoryMap "" displayRevisionsFlag displayMaturityLevelsFlag 
        mainLoop
                            
  | inp =~ "^\\:he"       = help >> mainLoop
                            
  | inp =~ "^\\:commands" = commands >> mainLoop
  

  | inp =~ "^\\:init" = do
    initCommand 
    showCommand
    mainLoop 

  | inp =~ "^\\:save" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get 
    liftIO $ saveToFile repositoryMap
    mainLoop
    
  | inp =~ "^\\:load" = do
    RepositoryMapState _ selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryMapState ( loadRepositoryMapFromFile ) selectedRepository displayRevisionsFlag displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:editBranch" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    branchVersionInput <- getInputLine "\tEnter version of the branch to edit: "
    case branchVersionInput of
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag
      Just stringBranchVersion -> if (null stringBranchVersion) then do
        outputStrLn $ "Version cannot be empty. Aborting operation"
        put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag
        else 
          let branchVersion = stringToVersion stringBranchVersion in 
          if (isRevision branchVersion) then do 
            outputStrLn $ "You should enter branch version. Aborting operation"
            put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag
          else do 
            newContentInput <- getInputLineWithInitial 
              "\tEnter new contents of the branch: " 
              ( getRepositoryContentByVersion branchVersion (fromJust $ M.lookup selectedRepository repositoryMap), "" )
            case newContentInput of
              Nothing -> put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag  
              Just newContent -> put $ RepositoryMapState 
                  (M.insert selectedRepository 
                            (editBranch branchVersion newContent (fromJust $ M.lookup selectedRepository repositoryMap ))
                            repositoryMap ) 
                  selectedRepository 
                  displayRevisionsFlag 
                  displayMaturityLevelsFlag
          {-showContentsCommand-}
    mainLoop

  | inp =~ "^\\:showContent" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    versionInput <- getInputLine "\tEnter version of the node to show: "
    case versionInput of 
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
      Just stringVersion -> let 
        version = stringToVersion stringVersion 
        content = getRepositoryContentByVersion version (fromJust $ M.lookup selectedRepository repositoryMap ) 
        in ( do
          outputStrLn $ show content
          put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
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

  | inp =~ "^\\:reSnapshot" = do
    newCommand reSnapshot "\tEnter version of the snapshot you want to be re-created with the same maturity level: "
    showCommand
    mainLoop

  | inp =~ "^\\:promoteSnapshot " = do
    newCommand promoteSnapshot "\tEnter version of the snapshot you want to be promoted: "
    showCommand
    mainLoop

  | inp =~ "^\\:toggleRevisions" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryMapState repositoryMap selectedRepository (not displayRevisionsFlag) displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:toggleMaturityLevels" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag ( not displayMaturityLevelsFlag )
    showCommand
    mainLoop

  | inp =~ "^\\:new" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    repositoryNameInput <- getInputLine "\tEnter name of the repository to create: "
    case repositoryNameInput of 
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
      Just repositoryName -> 
        put $ RepositoryMapState ( M.insert repositoryName initialRepository repositoryMap ) selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
    showCommand
    mainLoop

  | inp =~ "^\\:edit" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    selectedRepositoryInput <- getInputLine "\tEnter name of the repository to edit: "
    case selectedRepositoryInput of
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
      Just selectedRepositoryName ->
        let selectedRepositoryNameTrimmed = trim selectedRepositoryName in
        if elem selectedRepositoryNameTrimmed (M.keys repositoryMap)
          then put $ RepositoryMapState repositoryMap selectedRepositoryNameTrimmed displayRevisionsFlag displayMaturityLevelsFlag 
          else do 
            liftIO $ putStrLn $ "No repository with the name '" ++ selectedRepositoryNameTrimmed ++ "' found in repositories map"
            put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
    mainLoop
  
  | inp =~ "^\\:rename" = do
    RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag <- get
    selectedRepositoryInput <- getInputLine "\tEnter name of the repository to rename: "
    case selectedRepositoryInput of
      Nothing -> do
        put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
        mainLoop
      Just selectedRepositoryName -> 
        let selectedRepositoryNameTrimmed = trim selectedRepositoryName in
        if not $ elem selectedRepositoryNameTrimmed (M.keys repositoryMap)
          then do
            liftIO $ putStrLn $ "No repository with the name '" ++ selectedRepositoryNameTrimmed ++ "' found in repositories map"
            put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
            mainLoop
          else do
            newRepositoryNameInput <- getInputLine "\tEnter new name of the repository: "
            case newRepositoryNameInput of
              Nothing -> do
                liftIO $ putStrLn $ "Repository name has not been changed"
                put $ RepositoryMapState repositoryMap selectedRepository displayRevisionsFlag displayMaturityLevelsFlag 
                mainLoop
              Just newRepositoryName -> 
                let newRepositoryNameTrimmed = trim newRepositoryName 
                    renamedRecord = fromJust $ M.lookup selectedRepositoryNameTrimmed repositoryMap
                    mapWithoutRenamedRecord = M.delete selectedRepositoryNameTrimmed repositoryMap 
                    newSelectedRepository = if selectedRepository == selectedRepositoryNameTrimmed 
                      then selectedRepositoryNameTrimmed 
                      else selectedRepository 
                in do 
                   put $ RepositoryMapState 
                         (M.insert newRepositoryNameTrimmed renamedRecord mapWithoutRenamedRecord)
                         newSelectedRepository
                         displayRevisionsFlag 
                         displayMaturityLevelsFlag 
                   mainLoop

  | inp =~ ":" = do
    outputStrLn $ "\nNo command \"" ++ inp ++ "\"\n"
    mainLoop
    
  | otherwise = handleInput inp

handleInput :: String -> InputT (StateT RepositoryMapState IO)()
handleInput inp = mainLoop

mainLoop :: InputT (StateT RepositoryMapState IO)()
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

main :: IO ((), RepositoryMapState)
main = do 
    greet 
    runStateT (runInputT mySettings mainLoop) $ RepositoryMapState initialRepositoryMap "" defaultDisplayRevisionsFlagValue defaultDisplayMaturityLevelsFlagValue
   
