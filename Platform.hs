module Platform where 

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