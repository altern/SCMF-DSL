module RepositoryMap where 

import qualified Data.Map as M
import Repository 
import Data.Maybe
import RoseTree
import Data.Key

type RepositoryMap = M.Map String Repository
initialRepositoryMap :: RepositoryMap 
initialRepositoryMap = M.fromList []

displayRepositoryMap :: RepositoryMap -> Bool -> Bool -> IO ()
displayRepositoryMap map displayRevisionsFlag displayMaturityLevelsFlag = 
    mapWithKeyM_ (\k v -> do { 
      putStrLn $ show k ++ " => "; 
      displayRepository v displayRevisionsFlag displayMaturityLevelsFlag
    } ) map
