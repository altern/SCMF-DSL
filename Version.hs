{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Version where 

import VersionNumber
import MaturityLevel
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Control.Applicative

data Version = MaturityVersion MaturityLevel VersionNumber  -- Dev/1.x.0, Test/1.x.3, User/1.x.4, User/2.5.1, ...
			| Version VersionNumber

instance JSON.ToJSON Version where
	toJSON version = 
		JSON.object [ "version" JSON..= (T.pack $ show version)]

-- instance JSON.FromJSON Version where
	-- parseJSON (JSON.Object v) =
		-- Version <$> v JSON..: "version"
	-- parseJSON _ = mzero

instance JSON.FromJSON Version where
    parseJSON (JSON.Object v) = liftM stringToVersion ( v JSON..: "version" )
    parseJSON _ = mzero
 
parseVersionFromString :: String -> Version
parseVersionFromString s = 
    let bs = BS.pack s in case parse JSON.json bs of
               (Done rest r) -> case AT.parseMaybe JSON.parseJSON r of
                    (Just x) -> x
                    Nothing -> initialVersion (NumberPlaceholder)
               _             -> initialVersion (NumberPlaceholder)

type VersionList = [Version]

instance Show Version where
	show version = versionToString version

versionToString :: Version -> String
versionToString (MaturityVersion maturityLevel versionNumber) = (show maturityLevel) ++ "/" ++ (versionNumberToString versionNumber)
versionToString (Version versionNumber) = (versionNumberToString versionNumber)

generateNewVersionNumber ( VersionCompound vc ) = ( VersionCompound (generateNewVersionCompound vc) )
generateNewVersionNumber ( VersionNumber vc vn ) = ( VersionNumber (generateNewVersionCompound vc) vn )

generateNewVersion :: Version -> Version
generateNewVersion ( Version vn ) = Version ( generateNewVersionNumber vn )
generateNewVersion ( MaturityVersion level vn ) = MaturityVersion level ( generateNewVersionNumber vn )

instance VersionOperations Version where 
	decrement           (Version vn)                        = Version (decrement vn)
	decrement           (MaturityVersion ml vn)             = MaturityVersion ml (decrement vn)
	decrementDimension  num (Version vn)                    = Version (decrementDimension num vn)
	decrementDimension  (Number 0) (MaturityVersion ml vn)  = MaturityVersion (decrementMaturityLevel ml) vn
	decrementDimension  num (MaturityVersion ml vn)         = MaturityVersion ml (decrementDimension num vn)
	increment           (Version vn)                        = Version (increment vn)
	increment           (MaturityVersion ml vn)             = MaturityVersion ml (increment vn)
	incrementDimension  num (Version vn)                    = Version (incrementDimension num vn)
	incrementDimension  (Number 0) (MaturityVersion ml vn)  = MaturityVersion (incrementMaturityLevel ml) vn
	incrementDimension  num (MaturityVersion ml vn)         = MaturityVersion ml (incrementDimension num vn)

-- initialVersionNumber :: VersionNumber

initialVersion :: NumberOfDimensions -> Version
initialVersion dim = Version ( initialVersionNumber dim )

isInitialVersion :: Version -> Bool
isInitialVersion (Version v) = isInitialVersionNumber v
isInitialVersion (MaturityVersion _ v) = isInitialVersionNumber v

instance Eq Version where
	(Version vn1) == (Version vn2) = (vn1 == vn2)
	(MaturityVersion ml1 vn1) == (MaturityVersion ml2 vn2)      = (ml1 == ml2) && (vn1 == vn2)
	_ == _                                                      = False
	
instance Ord Version where
	(MaturityVersion ml1 vn1) `compare` (MaturityVersion ml2 vn2) = case vn1 == vn2 of 
		True -> ml1 `compare` ml2
		False -> vn1 `compare` vn2
	(MaturityVersion _ vn1) `compare` (Version vn2) = vn1 `compare` vn2
	(Version vn1) `compare` (MaturityVersion _ vn2) = vn1 `compare` vn2
	(Version vn1) `compare` (Version vn2) = vn1 `compare` vn2

parseMaturity :: Parser MaturityLevel
parseMaturity = 
		( string "Dev" >> return Dev)
	<|> ( string "Test" >> return Test)
	<|> ( string "User" >> return User)
	<|> ( string "ReleaseCandidate" >> return ReleaseCandidate)
	<|> ( string "Prod" >> return Prod)
	
stringToMaturity :: String -> MaturityLevel
stringToMaturity str = case (parseOnly parseMaturity $ BS.pack str) of
	Right a -> a
	Left _ -> Dev

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
	Left _ -> Version ( initialVersionNumber (Number 0) )

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
