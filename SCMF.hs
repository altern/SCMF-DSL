data VersionCompound = NumberPlaceholder                    -- X
                     | Number Int                           -- 1, 2, 3, ..., 45, ... 
                     deriving (Show)
                     
data VersionNumber = VersionCompound VersionCompound
             | SubVersion VersionNumber VersionCompound
             deriving (Show)
                        
data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Stable
                   deriving (Show)

data Version = MaturityVersion MaturityLevel VersionNumber
                deriving (Show)

data VersionNumberFilterCompound = FilterNumberPlaceholder
                           | FilterNumber Int
                           | EitherPlaceholderOrNumber
                           | AllNumbers
                           deriving (Show)

data VersionNumberFilter = VersionNumberFilterCompound VersionNumberFilterCompound
                   | SubFilter VersionNumberFilter VersionNumberFilterCompound
                   deriving (Show)

data VersionFilter = MaturityVersionFilter MaturityLevel VersionNumberFilter
                   deriving (Show)

-- data VersionRange

data ConnectionType = FTP
                    | LOCAL
                    | SFTP
                    | SSH
                    | RSYNC
                    deriving (Show)

data ConnectionProperty = Host String
                        | Port Int
                        | Username String
                        | Password String
                        | RemoteDirectory String
                        deriving (Show)

data Connection = Connection ConnectionType [ConnectionProperty]
                deriving (Show)

data Platform = PlatformName String Connection
                deriving (Show)

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
                
data Project = Project String Repository
              deriving (Show)