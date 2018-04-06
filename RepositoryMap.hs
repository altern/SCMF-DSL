module RepositoryMap where 

import qualified Data.Map as M
import Repository 
import Data.Maybe
import RoseTree
import Data.Key

type RepositoryMap = M.Map String Repository
initialRepositoryMap :: RepositoryMap 
initialRepositoryMap = M.fromList [("", initialRepository)]

displayRepositoryMap :: RepositoryMap -> IO ()
displayRepositoryMap map = mapWithKeyM_ (\k v -> do { putStrLn $ show k ++ " => "; displayTree v } ) map
