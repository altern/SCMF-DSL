{-# LANGUAGE OverloadedStrings #-}

module Version where 

import VersionNumber
import MaturityLevel
import qualified Data.Aeson as JSON
import qualified Data.Text as T

data Version = MaturityVersion MaturityLevel VersionNumber  -- Dev/1.x.0, Test/1.x.3, User/1.x.4, User/2.5.1, ...
             | Version VersionNumber

instance JSON.ToJSON Version where
    toJSON version = 
        JSON.object [ "version" JSON..= (T.pack $ show version)]

-- instance JSON.FromJSON Version where
    -- parseJSON (Object v) =
        -- Version <$> v .: "firstName"
               -- <*> v .: "lastName"
               -- <*> v .: "age"
               -- <*> v .: "likesPizza"
    -- parseJSON _ = mzero

type VersionList = [Version]

instance Show Version where
    show version = versionToString version

-- data VersionCompound = NumberPlaceholder 
                     -- | Number Int          
                     -- deriving (Show)

-- data VersionNumber = VersionCompound VersionCompound
       -- | VersionNumber VersionCompound VersionNumber
       -- deriving (Show)

versionToString :: Version -> String
versionToString (MaturityVersion maturityLevel versionNumber) = (show maturityLevel) ++ "/" ++ (versionNumberToString versionNumber)
versionToString (Version versionNumber) = (versionNumberToString versionNumber)

generateNewVersionCompound :: VersionCompound -> VersionCompound
generateNewVersionCompound NumberPlaceholder = NumberPlaceholder
generateNewVersionCompound (Number n) = (Number (n + 1))

generateNewVersionNumber ( VersionCompound vc ) = ( VersionCompound (generateNewVersionCompound vc) )
generateNewVersionNumber ( VersionNumber vc vn ) = ( VersionNumber (generateNewVersionCompound vc) vn )

generateNewVersion :: Version -> Version
generateNewVersion ( Version vn ) = Version ( generateNewVersionNumber vn )
generateNewVersion ( MaturityVersion level vn ) = MaturityVersion level ( generateNewVersionNumber vn )

-- initialVersionNumber :: VersionNumber

initialVersionCompound :: VersionCompound
initialVersionCompound = Number 0

-- initialVersion :: Version
-- initialVersion = Version (Number 0)

isInitialVersionCompound :: VersionCompound -> Bool
isInitialVersionCompound ( Number 0 ) = True

instance Eq Version where
    (Version vn1) == (Version vn2) = (vn1 == vn2)
    (MaturityVersion level1 vn1) == (MaturityVersion level2 vn2)             = (level1 == level2) && (vn1 == vn2)
    _ == _                                                     = False
    
instance Ord Version where
    (MaturityVersion level1 vn1) `compare` (MaturityVersion level2 vn2) = case vn1 == vn2 of 
        True -> level1 `compare` level2
        False -> vn1 `compare` vn2
    (MaturityVersion _ vn1) `compare` (Version vn2) = vn1 `compare` vn2
    (Version vn1) `compare` (MaturityVersion _ vn2) = vn1 `compare` vn2
    (Version vn1) `compare` (Version vn2) = vn1 `compare` vn2

-- EXAMPLES

vc1 :: VersionCompound
vc1 = NumberPlaceholder

vc2 :: VersionCompound
vc2 = (Number 1)

vc3 :: VersionCompound
vc3 = (Number 2)

v4 :: Version
v4 = MaturityVersion Dev ( VersionCompound ( Number 3 ) )

v5 :: Version
v5 = MaturityVersion ReleaseCandidate ( VersionCompound ( Number 50 ) )
