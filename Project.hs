module Project where

import ArtifactTree

data RepositoryType = SVN
                    | GIT
                    | HG
                    | DARCS
                    | CVS
                    | P4
                    | ACCUREV
                    | BAZAAR
                    | TFS
                    | VSS
                    | SCCS
                    | RCS
                    | FILESYSTEM
                    deriving (Show)
                    
data RepositoryProperty = RepositoryHost String
                         | RepositoryPort Int
                         | RepositoryUsername String
                         | RepositoryPassword String
                        deriving (Show)
                        
data Repository = Repository RepositoryType [RepositoryProperty]
                deriving (Show)
    
type ProjectName = String 

data Project = Project ProjectName Repository ArtifactTree
              deriving (Show)