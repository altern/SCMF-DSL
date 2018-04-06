module RepositoryMap where 

import qualified Data.Map as M
import Repository 

type RepositoryMap = M.Map String Repository
initialRepositoryMap :: RepositoryMap 
initialRepositoryMap = M.fromList [("", initialRepository)]
