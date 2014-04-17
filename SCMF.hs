---- VERSION data types ----

data VersionCompound = NumberPlaceholder                    -- X
                     | Number Int                           -- 1, 2, 3, ..., 45, ... 
                     deriving (Show)
                     
data VersionNumber = VersionCompound VersionCompound        -- X, 1, 2, 3, ... , 45, ...
             | SubVersion VersionNumber VersionCompound     -- X, X.X, X.X.X, X.X.X.X, ... , X.1, X.2, ..., X.45, ..., 1.X.23,
             deriving (Show)
                        
data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Stable
                   deriving (Show)

data Version = MaturityVersion MaturityLevel VersionNumber  -- Dev/1.x.0, Test/1.x.3, User/1.x.4, User/2.5.1, ...
                deriving (Show)

---- VERSION FILTER data types ----

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

-- TODO: implement data VersionRange 

---- PLATFORM data types ----

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

---- PROJECT data types ----

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
              