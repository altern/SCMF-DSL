module RepositoryMap where 

import qualified Data.Map as M
import Repository 
import Version
import Data.Maybe
import RoseTree
import Data.Key

type RepositoryMap = M.Map String Repository
initialRepositoryMap :: RepositoryMap 
initialRepositoryMap = M.fromList []

displayRepositoryMap :: RepositoryMap -> Version -> Bool -> Bool -> IO ()
displayRepositoryMap map selectedVersion displayRevisionsFlag displayMaturityLevelsFlag = 
    mapWithKeyM_ (\k v -> do { 
      putStrLn $ show k ++ " => "; 
      displayRepository v selectedVersion displayRevisionsFlag displayMaturityLevelsFlag
    } ) map
