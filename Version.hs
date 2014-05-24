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

versionToString :: Version -> String
versionToString (MaturityVersion maturityLevel versionNumber) = (show maturityLevel) ++ "/" ++ (versionNumberToString versionNumber)
versionToString (Version versionNumber) = (versionNumberToString versionNumber)

generateNewVersion :: Version -> Version
generateNewVersion (Version NumberPlaceholder) = Version NumberPlaceholder
generateNewVersion (Version (Number n)) = Version (Number (n + 1))
generateNewVersion (MaturityVersion level NumberPlaceholder) = MaturityVersion level NumberPlaceholder
generateNewVersion (MaturityVersion level (Number n)) = MaturityVersion level (Number (n + 1))

initialVersion :: Version
initialVersion = Version (Number 0)

isInitialVersion :: Version -> Bool
isInitialVersion ( Version ( Number 0 ) ) = True

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

v1 :: Version
v1 = Version NumberPlaceholder

v2 :: Version
v2 = Version (Number 1)

v3 :: Version
v3 = Version (Number 2)

v4 :: Version
v4 = MaturityVersion Dev (Number 3)

v5 :: Version
v5 = MaturityVersion ReleaseCandidate (Number 50)
