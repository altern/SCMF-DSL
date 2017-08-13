{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Version where 

import VersionNumber
import MaturityLevel
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Control.Applicative

data Version = MaturityVersion MaturityLevel VersionNumber  -- Dev/1.x.0, Test/1.x.3, User/1.x.4, User/2.5.1, ...
     | Version VersionNumber

instance JSON.ToJSON Version where
    toJSON version = 
        JSON.object [ "version" JSON..= (T.pack $ show version)]

instance JSON.FromJSON Version where
    parseJSON (JSON.Object v) = liftM stringToVersion ( v JSON..: "version" )
    parseJSON _ = mzero
 
parseVersionFromString :: String -> Version
parseVersionFromString s = 
    let bs = BS.pack s in case parse JSON.json bs of
               (Done rest r) -> case AT.parseMaybe JSON.parseJSON r of
                    (Just x) -> x
                    Nothing -> Version $ VersionNumber [Nothing]
               _             -> Version $ VersionNumber [Nothing]

type VersionList = [Version]

instance Show Version where
    show version = toString version

instance ToString Version where
    toString (MaturityVersion maturityLevel versionNumber) = (show maturityLevel) ++ "/" ++ (toString versionNumber)
    toString (Version versionNumber) = (toString versionNumber)

instance VersionOperations Version where 
    decrement           (Version vn)                        = Version (decrement vn)
    decrement           (MaturityVersion ml vn)             = MaturityVersion ml (decrement vn)
    decrementDimension  num (Version vn)                    = Version (decrementDimension num vn)
    decrementDimension  0 (MaturityVersion ml vn)           = MaturityVersion (decrement ml) vn
    decrementDimension  num (MaturityVersion ml vn)         = MaturityVersion ml (decrementDimension num vn)
    increment           (Version vn)                        = Version (increment vn)
    increment           (MaturityVersion ml vn)             = MaturityVersion ml (increment vn)
    incrementDimension  num (Version vn)                    = Version (incrementDimension num vn)
    incrementDimension  0 (MaturityVersion ml vn)    = MaturityVersion (increment ml) vn
    incrementDimension  num (MaturityVersion ml vn)         = MaturityVersion ml (incrementDimension num vn)
    getNumberOfDimensions (Version vn)                      = getNumberOfDimensions vn
    getNumberOfDimensions (MaturityVersion ml vn)           = getNumberOfDimensions vn
    appendDimension     (Version vn)                        = Version (appendDimension vn)
    appendDimension     (MaturityVersion ml vn)             = MaturityVersion ml (appendDimension vn)
    
instance VersionDetection Version where
        isInitial (Version v) = isInitial v
        isInitial (MaturityVersion _ v) = isInitial v
        isExperimentalBranch (Version v) = isExperimentalBranch v
        isExperimentalBranch (MaturityVersion _ v) = isExperimentalBranch v
        isReleaseBranch (Version v) = isReleaseBranch v
        isReleaseBranch (MaturityVersion _ v) = isReleaseBranch v
        isSupportBranch (Version v) = isSupportBranch v
        isSupportBranch (MaturityVersion _ v) = isSupportBranch v
        isExperimentalSnapshot (Version v) = isExperimentalSnapshot v
        isExperimentalSnapshot (MaturityVersion _ v) = isExperimentalSnapshot v
        isReleaseSnapshot (Version v) = isReleaseSnapshot v
        isReleaseSnapshot (MaturityVersion _ v) = isReleaseSnapshot v
        isSupportSnapshot (Version v) = isSupportSnapshot v
        isSupportSnapshot (MaturityVersion _ v) = isSupportSnapshot v

{-incrementReleaseNumberForVersion :: Version -> Version-}
{-incrementReleaseNumberForVersion (Version v) = Version (incrementReleaseNumber v)-}
{-incrementReleaseNumberForVersion (MaturityVersion ml v) = MaturityVersion ml ( incrementReleaseNumber v )-}

{-incrementSupportNumberForVersion :: Version -> Version-}
{-incrementSupportNumberForVersion (Version v) = Version (incrementSupportNumber v)-}
{-incrementSupportNumberForVersion (MaturityVersion ml v) = MaturityVersion ml ( incrementSupportNumber v )-}

{-freezeExperimentalVersion :: Version -> Version-}
{-freezeExperimentalVersion (Version v) = Version (freezeExperimental v)-}
{-freezeExperimentalVersion (MaturityVersion ml v) = MaturityVersion ml ( freezeExperimental v )-}

{-freezeReleaseVersion :: Version -> Version-}
{-freezeReleaseVersion (Version v) = Version (freezeRelease v)-}
{-freezeReleaseVersion (MaturityVersion ml v) = MaturityVersion ml ( freezeRelease v )-}

{-freezeSupportVersion :: Version -> Version-}
{-freezeSupportVersion (Version v) = Version (freezeSupport v)-}
{-freezeSupportVersion (MaturityVersion ml v) = MaturityVersion ml ( freezeSupport v )-}

selectLatestVersion :: [Version] -> Version
selectLatestVersion [] = Version $ VersionNumber []
selectLatestVersion (x:xs) = max x (selectLatestVersion xs)

{-instance FreezeDimension Version where-}
        {-freezeDimensionByNum (Number 0) (Version v) = MaturityVersion Dev v-}
        {-freezeDimensionByNum num (Version v) = Version (freezeDimensionByNum num v)-}
        {-freezeDimensionByNum num (MaturityVersion ml v) = MaturityVersion ml (freezeDimensionByNum num v)-}

instance Eq Version where
    (Version vn1) == (Version vn2) = (vn1 == vn2)
    (Version vn1) == (MaturityVersion ml vn2) = (ml == Dev) && vn1 == vn2
    (MaturityVersion ml vn1) == (Version vn2) = (ml == Dev) && vn1 == vn2
    (MaturityVersion ml1 vn1) == (MaturityVersion ml2 vn2)      = ( ml1 == ml2 ) && (vn1 == vn2)
    
instance Ord Version where
    (MaturityVersion ml1 vn1) `compare` (MaturityVersion ml2 vn2) = case vn1 == vn2 of 
        True -> ml1 `compare` ml2
        False -> vn1 `compare` vn2
    (MaturityVersion _ vn1) `compare` (Version vn2) = vn1 `compare` vn2
    (Version vn1) `compare` (MaturityVersion _ vn2) = vn1 `compare` vn2
    (Version vn1) `compare` (Version vn2) = vn1 `compare` vn2

instance MakeDimensional Version where
	makeNDimensional dim (Version version) = Version (makeNDimensional dim version)
	makeNDimensional dim (MaturityVersion ml version) = MaturityVersion ml (makeNDimensional dim version)

parseVersion :: Parser Version
parseVersion = do { 
        maturity <- parseMaturity
      ; char '/'
      ; version <- parseVersionNumber
      ; return $ MaturityVersion maturity version
    } 
    <|> do { 
        version <- parseVersionNumber
      ; return $ Version version
    } 

stringToVersion :: String -> Version
stringToVersion str = case (parseOnly parseVersion $ BS.pack str) of
    Right a -> a
    Left _ -> Version ( VersionNumber [Nothing] )


