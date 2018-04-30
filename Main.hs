{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances#-}
module Main where

import RoseTree
import Version
import VersionNumber
import Repository
import RepositoryMap
import Platform
import FileOperation
import UserOperation
import MaturityLevel
import Util

import Text.Regex.PCRE
import System.Console.Haskeline
import System.IO
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List 
import qualified Data.Map as M
import Data.Maybe 
import Data.Time.Clock.POSIX

data RepositoryMapState = RepositoryMapState {
  repositoryMap :: RepositoryMap,
  selectedRepository :: String,
  selectedVersion :: Version,
  displayRevisionsFlag :: Bool,
  displayMaturityLevelsFlag :: Bool
}

type SearchFunc = RepositoryMapState -> String -> [Completion]

type MS = ReaderT SearchFunc (StateT RepositoryMapState IO)

defaultDisplayRevisionsFlagValue :: Bool
defaultDisplayRevisionsFlagValue = True

defaultDisplayMaturityLevelsFlagValue :: Bool
defaultDisplayMaturityLevelsFlagValue = False

instance MonadState s m => MonadState s (InputT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadReader s m => MonadReader s (InputT m) where
    reader = lift . reader
    local f = mapInputT (local f)

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

editBranchCommand           = (":editBranch","edits contents of the specific branch")
showContentCommand          = (":showContent","shows contents of the specific branch")
newSupportBranchCommand     = (":newSupportBranch","adds new support branch to the version tree")
newReleaseBranchCommand     = (":newReleaseBranch","adds new release branch to the version tree")
newRevisionCommand          = (":newRevision","adds new revision to the version tree")
promoteSnapshotCommand      = (":promoteSnapshot","promotes specific snapshot to the next maturity level")
newSupportSnapshotCommand   = (":newSupportSnapshot","adds new support snapshot to the version tree")
newReleaseSnapshotCommand   = (":newReleaseSnapshot","adds new release snapshot to the version tree")
reSnapshotCommand           = (":reSnapshot","recreates snapshot with the same maturity level")

repositoryCommands = M.fromList [
            (":help","displays this help"), 
            (":q","quits repository editing mode"), 
            (":commands","displays the list of available commands"), 
            (":init", "initializes repository with an initial"),
            (":show", "displays version tree of the currently selected repository"),
            (":save", "saves version tree of the currently selected repository into a JSON file with repository name"),
            (":load", "loads repository from JSON file with repository name"),
            (":print", "prints repository as a valid Haskell code"),
            editBranchCommand,
            showContentCommand,
            newSupportBranchCommand, 
            newReleaseBranchCommand, 
            newRevisionCommand, 
            promoteSnapshotCommand,
            newSupportSnapshotCommand, 
            newReleaseSnapshotCommand,
            reSnapshotCommand,
            (":selectVersion","selects version to be currently selected")
  ]

versionCommands = M.fromList [
            (":q", "quits version selection mode")
  ]

mainlineBranchCommands = M.fromList [
            editBranchCommand,
            showContentCommand,
            newSupportBranchCommand
  ]

supportBranchCommands = M.fromList [
            editBranchCommand,
            showContentCommand,
            newReleaseBranchCommand,
            newRevisionCommand,
            newSupportSnapshotCommand
  ]

releaseBranchCommands = M.fromList [
            editBranchCommand,
            showContentCommand,
            newRevisionCommand,
            newReleaseSnapshotCommand
  ]

supportSnapshotCommands = M.fromList [
            showContentCommand,
            promoteSnapshotCommand,
            reSnapshotCommand
  ]

releaseSnapshotCommands = M.fromList [
            showContentCommand,
            promoteSnapshotCommand,
            reSnapshotCommand
  ]

revisionCommands = M.fromList [
            showContentCommand
  ]

versionToCommands :: Version -> M.Map String String
versionToCommands version = 
      if isInitial version then mainlineBranchCommands
      else if isSupportBranch version then supportBranchCommands
      else if isReleaseBranch version then releaseBranchCommands
      else if isSupportSnapshot version then supportSnapshotCommands
      else if isReleaseSnapshot version then releaseSnapshotCommands
      else if isRevision version then revisionCommands
      else M.fromList []

topLevelSearchFunc :: SearchFunc
topLevelSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion 
      $ filter (str `isPrefixOf`)
      $ if null selectedRepository 
          then M.keys repositoryMapCommands 
          else if isInitial selectedVersion 
               then M.keys repositoryCommands
               else M.keys $ versionToCommands selectedVersion

editSearchFunc :: SearchFunc 
editSearchFunc (RepositoryMapState repositoryMap selectedRepository _ _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ M.keys repositoryMap

editBranchSearchFunc :: SearchFunc 
editBranchSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString (( findAllSupportBranches repository  ) ++ (findAllReleaseBranches repository))
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

showContentSearchFunc :: SearchFunc 
showContentSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString (( findAllSupportBranches repository  ) 
                    ++ (findAllReleaseBranches repository) 
                    ++ (findAllSupportSnapshots repository) 
                    ++ (findAllReleaseSnapshots repository) 
                    ++ (findAllRevisions repository))
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

newSupportBranchSearchFunc :: SearchFunc 
newSupportBranchSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString ( ( findAllMainlines repository ) ) 
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

newReleaseBranchSearchFunc :: SearchFunc 
newReleaseBranchSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString ( ( findAllMainlines repository ) ++ ( findAllSupportBranches repository ) )
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

newSupportSnapshotSearchFunc :: SearchFunc 
newSupportSnapshotSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString ( ( findAllSupportBranches repository ) )
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

newReleaseSnapshotSearchFunc :: SearchFunc 
newReleaseSnapshotSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString ( ( findAllReleaseBranches repository ) )
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

newRevisionSearchFunc :: SearchFunc 
newRevisionSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString ( ( findAllMainlines repository ) ++ ( findAllSupportBranches repository ) ++ ( findAllReleaseBranches repository ) )
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

reSnapshotSearchFunc :: SearchFunc 
reSnapshotSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString ( ( findAllSupportSnapshots repository ) ++ ( findAllReleaseSnapshots repository ) )
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

promoteSnapshotSearchFunc :: SearchFunc 
promoteSnapshotSearchFunc (RepositoryMapState repositoryMap selectedRepository selectedVersion _ _) str = map simpleCompletion $ filter (str `isPrefixOf`)
      $ map toString ( ( findAllSupportSnapshots repository ) ++ ( findAllReleaseSnapshots repository ) )
        where repository = (fromJust $ M.lookup selectedRepository repositoryMap)

mySettings :: Settings MS 
mySettings = Settings { historyFile = Just "hist"
                      , complete = completeWord Nothing "\t" $ \str -> do 
                          data_ <- get
                          searchFunc <- ask
                          return $ searchFunc data_ str
                      , autoAddHistory = True
                      }

help :: InputT MS ()
help = commands
       
commands :: InputT MS ()
commands = do 
        (RepositoryMapState _ selectedRepository selectedVersion _ _ ) <- get
        liftIO $ mapM_ putStrLn $ if null selectedRepository 
            then M.elems $ M.mapWithKey (\k v -> k ++ "\t - " ++ v) repositoryMapCommands
            else M.elems $ M.mapWithKey (\k v -> k ++ "\t - " ++ v) repositoryCommands
       
showCommand :: InputT MS ()
showCommand = do
  RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
  if null selectedRepository 
    then liftIO $ displayRepositoryMap repositoryMap displayRevisionsFlag displayMaturityLevelsFlag 
    else liftIO $ let repository = (fromJust $ M.lookup selectedRepository repositoryMap) in
      do putStrLn $ show selectedRepository ++ " => "
         displayRepository repository displayRevisionsFlag displayMaturityLevelsFlag

initCommand :: InputT MS  ()
initCommand = do
  RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <-get
  if (null selectedRepository) then
    put $ RepositoryMapState initialRepositoryMap "" initialVersion displayRevisionsFlag displayMaturityLevelsFlag
  else
    put $ RepositoryMapState 
          (M.adjust (\x->initialRepository) selectedRepository repositoryMap)
          selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag

newCommand :: (Version -> Repository -> Timestamp -> Repository) -> String -> SearchFunc -> InputT MS () 
newCommand newFunc message searchFunc = do
  RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
  timestamp <- liftIO $ getPOSIXTime
  versionInput <- if null message then liftIO $ return $ Just $ toString initialVersion else local (\_ -> searchFunc) $ getInputLine message
  let timestampStr = timestampToString timestamp 
  case versionInput of
    Nothing -> put $ RepositoryMapState 
      ( M.insert selectedRepository 
                 (newFunc initialVersion (fromJust $ M.lookup selectedRepository repositoryMap ) timestampStr) 
                 repositoryMap)
      selectedRepository 
      selectedVersion
      displayRevisionsFlag 
      displayMaturityLevelsFlag
    Just stringVersion -> put $ RepositoryMapState 
      ( M.insert selectedRepository 
                 (newFunc (stringToVersion stringVersion) (fromJust $ M.lookup selectedRepository repositoryMap ) timestampStr)
                 repositoryMap) 
      selectedRepository 
      selectedVersion
      displayRevisionsFlag 
      displayMaturityLevelsFlag

parseInput :: String -> InputT MS ()
parseInput inp
  | inp =~ "^\\:q" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    if null selectedRepository 
      then return () 
      else do 
        put $ RepositoryMapState repositoryMap "" selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
        mainLoop
                            
  | inp =~ "^\\:he"       = help >> mainLoop
                            
  | inp =~ "^\\:commands" = commands >> mainLoop

  | inp =~ "^\\:init" = do
    initCommand 
    showCommand
    mainLoop 

  | inp =~ "^\\:save" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get 
    if (null selectedRepository) then do
      liftIO $ saveToFile repositoryMap
      outputStrLn $ "Saved repository map to file"
    else do
      liftIO $ saveToFileWithName (selectedRepository ++ ".json") (fromJust $ M.lookup selectedRepository repositoryMap)
      outputStrLn $ "Saved repository file " ++ (show (selectedRepository ++ ".json"))
    mainLoop
    
  | inp =~ "^\\:load" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    if (null selectedRepository) then
      put $ RepositoryMapState ( loadRepositoryMapFromFile ) selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag
    else
      put $ RepositoryMapState
            (M.adjust (\x->loadRepositoryFromFileWithName $ selectedRepository ++ ".json") selectedRepository repositoryMap)
            selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:editBranch" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    branchVersionInput <- local (\_ -> editBranchSearchFunc) $ getInputLine "\tEnter version of the branch to edit: "
    case branchVersionInput of
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag
      Just stringBranchVersion -> if (null stringBranchVersion) then do
        outputStrLn $ "Version cannot be empty. Aborting operation"
        put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag
        else 
          let branchVersion = stringToVersion stringBranchVersion in 
          if (isRevision branchVersion) then do 
            outputStrLn $ "You should enter branch version. Aborting operation"
            put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag
          else do 
            newContentInput <- getInputLineWithInitial 
              "\tEnter new contents of the branch: " 
              ( getRepositoryNodeContentByVersion branchVersion (fromJust $ M.lookup selectedRepository repositoryMap), "" )
            case newContentInput of
              Nothing -> put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag  
              Just newContent -> put $ RepositoryMapState 
                  (M.insert selectedRepository 
                            (editBranch branchVersion newContent (fromJust $ M.lookup selectedRepository repositoryMap ))
                            repositoryMap ) 
                  selectedRepository 
                  selectedVersion
                  displayRevisionsFlag 
                  displayMaturityLevelsFlag
          {-showContentsCommand-}
    mainLoop

  | inp =~ "^\\:showContent" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    versionInput <- local (\_ -> showContentSearchFunc ) $ getInputLine "\tEnter version of the node to show: "
    case versionInput of 
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
      Just stringVersion -> let 
        version = stringToVersion stringVersion 
        content = getRepositoryNodeContentByVersion version (fromJust $ M.lookup selectedRepository repositoryMap ) 
        in ( do
          outputStrLn $ show content
          put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
        )
    mainLoop

  | inp =~ "^\\:selectVersion" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    versionInput <- local (\_ -> showContentSearchFunc ) $ getInputLine "\tEnter version: "
    case versionInput of 
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
      Just stringVersion -> let 
        version = stringToVersion stringVersion 
        in ( do 
          outputStrLn $ "Selected version: " ++ toString version
          put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
        ) 
    mainLoop

  | inp =~ "^\\:show" = showCommand >> mainLoop 

  | inp =~ "^\\:newSupportBranch" = do
    newCommand newSupportBranch "" newSupportBranchSearchFunc 
    showCommand
    mainLoop

  | inp =~ "^\\:newReleaseBranch" = do
    newCommand newReleaseBranch "\tEnter version, which will be used to append new release branch to: " newReleaseBranchSearchFunc
    showCommand
    mainLoop

  | inp =~ "^\\:newSupportSnapshot" = do
    newCommand newSupportSnapshot "\tEnter version, which will be used to append new support snapshot to: " newSupportSnapshotSearchFunc
    showCommand
    mainLoop

  | inp =~ "^\\:newReleaseSnapshot" = do
    newCommand newReleaseSnapshot "\tEnter version, which will be used to append new release snapshot to: " newReleaseSnapshotSearchFunc
    showCommand
    mainLoop

  | inp =~ "^\\:newRevision" = do
    newCommand newRevision "\tEnter version, which will be used to append new revision to: " newRevisionSearchFunc 
    showCommand
    mainLoop

  | inp =~ "^\\:reSnapshot" = do
    newCommand reSnapshot "\tEnter version of the snapshot you want to be re-created with the same maturity level: " reSnapshotSearchFunc
    showCommand
    mainLoop

  | inp =~ "^\\:promoteSnapshot " = do
    newCommand promoteSnapshot "\tEnter version of the snapshot you want to be promoted: " promoteSnapshotSearchFunc
    showCommand
    mainLoop

  | inp =~ "^\\:toggleRevisions" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryMapState repositoryMap selectedRepository selectedVersion (not displayRevisionsFlag) displayMaturityLevelsFlag
    showCommand
    mainLoop

  | inp =~ "^\\:toggleMaturityLevels" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag ( not displayMaturityLevelsFlag )
    showCommand
    mainLoop

  | inp =~ "^\\:new" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    repositoryNameInput <- getInputLine "\tEnter name of the repository to create: "
    case repositoryNameInput of 
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
      Just repositoryName -> 
        put $ RepositoryMapState ( M.insert repositoryName initialRepository repositoryMap ) selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
    showCommand
    mainLoop

  | inp =~ "^\\:edit" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    selectedRepositoryInput <- local (\_ -> editSearchFunc) $ getInputLine "\tEnter name of the repository to edit: "
    case selectedRepositoryInput of
      Nothing -> put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
      Just selectedRepositoryName ->
        let selectedRepositoryNameTrimmed = trim selectedRepositoryName in
        if elem selectedRepositoryNameTrimmed (M.keys repositoryMap)
          then put $ RepositoryMapState repositoryMap selectedRepositoryNameTrimmed selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
          else do 
            liftIO $ putStrLn $ "No repository with the name '" ++ selectedRepositoryNameTrimmed ++ "' found in repositories map"
            put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
    mainLoop
  
  | inp =~ "^\\:rename" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    selectedRepositoryInput <- getInputLine "\tEnter name of the repository to rename: "
    case selectedRepositoryInput of
      Nothing -> do
        put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
        mainLoop
      Just selectedRepositoryName -> 
        let selectedRepositoryNameTrimmed = trim selectedRepositoryName in
        if not $ elem selectedRepositoryNameTrimmed (M.keys repositoryMap)
          then do
            liftIO $ putStrLn $ "No repository with the name '" ++ selectedRepositoryNameTrimmed ++ "' found in repositories map"
            put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
            mainLoop
          else do
            newRepositoryNameInput <- getInputLine "\tEnter new name of the repository: "
            case newRepositoryNameInput of
              Nothing -> do
                liftIO $ putStrLn $ "Repository name has not been changed"
                put $ RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag 
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
                         selectedVersion
                         displayRevisionsFlag 
                         displayMaturityLevelsFlag 
                   mainLoop

  | inp =~ ":print" = do
    RepositoryMapState repositoryMap selectedRepository selectedVersion displayRevisionsFlag displayMaturityLevelsFlag <- get
    liftIO $ print (fromJust $ M.lookup selectedRepository repositoryMap)
    mainLoop
   
  | inp =~ ":" = do
    outputStrLn $ "\nNo command \"" ++ inp ++ "\"\n"
    mainLoop
   
  | otherwise = handleInput inp

handleInput :: String -> InputT MS ()
handleInput inp = mainLoop

mainLoop :: InputT MS ()
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
    runStateT (runReaderT (runInputT mySettings mainLoop) topLevelSearchFunc) 
        $ RepositoryMapState initialRepositoryMap "" initialVersion defaultDisplayRevisionsFlagValue defaultDisplayMaturityLevelsFlagValue
   
