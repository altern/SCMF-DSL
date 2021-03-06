{-# LANGUAGE OverloadedStrings, DataKinds, DeriveAnyClass, DeriveGeneric #-}

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
import GHC.Generics (Generic)

data Version = MaturityVersion MaturityLevel VersionNumber  -- Dev/1.x.0, Test/1.x.3, User/1.x.4, User/2.5.1, ...
     | Version VersionNumber
    deriving (Show, JSON.ToJSON, JSON.FromJSON, Generic)
{-instance JSON.ToJSON Version where-}
    {-toJSON version = -}
        {-JSON.object [ "version" JSON..= (T.pack $ show version)]-}

{-instance JSON.FromJSON Version where-}
    {-parseJSON (JSON.Object v) = liftM stringToVersion ( v JSON..: "version" )-}
    {-parseJSON _ = mzero-}
 
{-parseVersionFromJSON :: String -> Version-}
{-parseVersionFromJSON s = -}
    {-let bs = BS.pack s in case parse JSON.json bs of-}
               {-(Done rest r) -> case AT.parseMaybe JSON.parseJSON r of-}
                    {-(Just x) -> x-}
                    {-Nothing  -> Version $ VersionNumber [Nothing]-}
               {-_             -> Version $ VersionNumber [Nothing]-}

type VersionList = [Version]

{-instance Show Version where-}
    {-show version = toString version-}

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
    incrementDimension  0 (MaturityVersion ml vn)           = MaturityVersion (increment ml) vn
    incrementDimension  num (MaturityVersion ml vn)         = MaturityVersion ml (incrementDimension num vn)
    freeze              (Version vn)                        = Version (freeze vn)
    freeze              (MaturityVersion ml vn)             = MaturityVersion ml (freeze vn)
    freezeDimension     num (Version vn)                    = Version (freezeDimension num vn)
    freezeDimension     0 (MaturityVersion ml vn)           = MaturityVersion (freeze ml) vn
    freezeDimension     num (MaturityVersion ml vn)         = MaturityVersion ml (freezeDimension num vn)

instance DimensionOperations Version where
    getNumberOfDimensions (Version vn)                      = getNumberOfDimensions vn
    getNumberOfDimensions (MaturityVersion ml vn)           = getNumberOfDimensions vn
    getActualNumberOfDimensions (Version vn)                = getActualNumberOfDimensions vn
    getActualNumberOfDimensions (MaturityVersion ml vn)     = getActualNumberOfDimensions vn
    appendDimension     (Version vn)                        = Version (appendDimension vn)
    appendDimension     (MaturityVersion ml vn)             = MaturityVersion ml (appendDimension vn)
    
instance VersionDetection Version where
        isInitial (Version v) = isInitial v
        isInitial (MaturityVersion _ v) = isInitial v
        isZero (Version v) = isZero v
        isZero (MaturityVersion _ v) = isZero v
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
        isRevision (Version v) = isRevision v
        isRevision (MaturityVersion _ v) = isRevision v

instance GenerateNew Version where
        generateNewReleaseBranch (Version v) = Version $ generateNewReleaseBranch v
        generateNewReleaseBranch (MaturityVersion ml v) = MaturityVersion ml $ generateNewReleaseBranch v
        generateNewSupportBranch (Version v) = Version $ generateNewSupportBranch v
        generateNewSupportBranch (MaturityVersion ml v) = MaturityVersion ml $ generateNewSupportBranch v
        generateNewReleaseSnapshot (Version v) = Version $ generateNewReleaseSnapshot v
        generateNewReleaseSnapshot (MaturityVersion ml v) = MaturityVersion ml $ generateNewReleaseSnapshot v
        generateNewSupportSnapshot (Version v) = Version $ generateNewSupportSnapshot v
        generateNewSupportSnapshot (MaturityVersion ml v) = MaturityVersion ml $ generateNewSupportSnapshot v
        generateNewRevision (Version v) = Version $ generateNewRevision v
        generateNewRevision (MaturityVersion ml v) = MaturityVersion ml $ generateNewRevision v
        generateNewExperimentalSnapshot (Version v) = Version $ generateNewExperimentalSnapshot v
        generateNewExperimentalSnapshot (MaturityVersion ml v) = MaturityVersion ml $ generateNewExperimentalSnapshot v
        generateNewVersion (Version v) = Version $ generateNewVersion v
        generateNewVersion (MaturityVersion ml v) = MaturityVersion ml $ generateNewVersion v
  
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
    (Version vn1) == (MaturityVersion ml vn2) = Dev == ml && vn1 == vn2
    (MaturityVersion ml vn1) == (Version vn2) = ml == Dev && vn1 == vn2
    (MaturityVersion ml1 vn1) == (MaturityVersion ml2 vn2) = (ml1 == ml2) && (vn1 == vn2)
    
instance Ord Version where
    (MaturityVersion ml1 vn1) `compare` (MaturityVersion ml2 vn2) = case vn1 == vn2 of 
        True -> ml1 `compare` ml2
        False -> vn1 `compare` vn2
    (MaturityVersion ml vn1) `compare` (Version vn2) = case vn1 == vn2 of 
        True -> ml `compare` Dev 
        False -> vn1 `compare` vn2
    (Version vn1) `compare` (MaturityVersion ml vn2) = case vn1 == vn2 of
        True -> Dev `compare` ml
        False -> vn1 `compare` vn2 
    (Version vn1) `compare` (Version vn2) = vn1 `compare` vn2

instance MakeDimensional Version where
    makeNDimensional dim (Version version) = Version (makeNDimensional dim version)
    makeNDimensional dim (MaturityVersion ml version) = MaturityVersion ml (makeNDimensional dim version)

instance GetParent Version where
    getParent (Version version) = Version (getParent version)
    getParent (MaturityVersion ml version) = Version (getParent version)

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

initialVersion :: Version
initialVersion = Version $ initialVersionNumber

zeroVersion :: Version
zeroVersion = Version $ zeroVersionNumber

toMaturityVersion :: Version -> Version
toMaturityVersion (Version v) = MaturityVersion Dev v
toMaturityVersion (MaturityVersion ml v) = MaturityVersion ml v

toVersion :: Version -> Version
toVersion (MaturityVersion Dev v) = Version v
toVersion (MaturityVersion ml v) = MaturityVersion ml v
toVersion (Version v) = Version v

promoteVersion :: Version -> Version
promoteVersion (Version v) = MaturityVersion (increment initialMaturity) (generateNewVersion v)
promoteVersion (MaturityVersion ml v) = MaturityVersion (increment ml) (generateNewVersion v)

promoteSupportVersion :: Version -> Version
promoteSupportVersion (Version v) = MaturityVersion (increment initialMaturity) (generateNewVersion v)
promoteSupportVersion (MaturityVersion User v) = MaturityVersion User (generateNewVersion v)
promoteSupportVersion (MaturityVersion ml v) = MaturityVersion (increment ml) (generateNewVersion v)

getMaturity :: Version -> MaturityLevel 
getMaturity (Version _) = Dev
getMaturity (MaturityVersion ml _) = ml

getVersionNumber :: Version -> VersionNumber
getVersionNumber (Version v) = v
getVersionNumber (MaturityVersion _ v) = v
