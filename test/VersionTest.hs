module VersionTest where 

import Test.HUnit
import VersionNumber
import MaturityLevel
import Version
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Test.AssertError

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

tests = test [ 
	"test A01"	~: "versionToString x"						~: "x"														~=? ( versionToString (Version ( VersionCompound NumberPlaceholder ) ) ),
	"test A02"	~: "versionToString x.x.x"					~: "x.x.x"													~=? ( versionToString (Version ( VersionNumber (NumberPlaceholder) ( VersionNumber (NumberPlaceholder) ( VersionCompound NumberPlaceholder ) ) ) ) ),
	"test A03"	~: "versionToString Dev/x.x.x"				~: "Dev/x.x.x"												~=? ( versionToString (MaturityVersion Dev ( VersionNumber (NumberPlaceholder) ( VersionNumber (NumberPlaceholder) ( VersionCompound NumberPlaceholder ) ) ) ) ),
	"test A04"	~: "versionToString Prod/1.0.3"				~: "Prod/1.0.3"												~=? ( versionToString (MaturityVersion Prod ( VersionNumber (Number 1) ( VersionNumber (Number 0) ( VersionCompound ( Number 3 ) ) ) ) ) ),
	"test B01"	~: "JSON.toJSON x"							~: "{\"version\":\"x\"}"									~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON ( Version ( VersionCompound NumberPlaceholder ) ) ),
--	"test C01"	~: "JSON.FromJSON x"						~: "x"														~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON ( Version ( VersionCompound NumberPlaceholder ) ) ),
--	"test C01"	~: "JSON.FromJSON x"						~: "x"														~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON ( Version ( VersionCompound NumberPlaceholder ) ) ),
	"test D01"	~: "stringToVersion x"						~: Version ( VersionCompound NumberPlaceholder)				~=? ( stringToVersion "x"  ),
	"test D02"	~: "stringToVersion 1"						~: Version ( VersionCompound (Number 1) )					~=? ( stringToVersion "1"  ),
	"test D03"	~: "stringToVersion 12"						~: Version ( VersionCompound (Number 12) )					~=? ( stringToVersion "12" ),
	"test D04"	~: "stringToVersion Dev/12"					~: MaturityVersion Dev ( VersionCompound (Number 12) )		~=? ( stringToVersion "Dev/12" ),
	"test E01"	~: "increment x"							~: "x"														~=? ( versionToString (increment $ stringToVersion "x" ) ),
	"test E02"	~: "increment 1"							~: "2"														~=? ( versionToString (increment $ stringToVersion "1" ) ),
	"test E03"	~: "increment 13"							~: "14"														~=? ( versionToString (increment $ stringToVersion "13" ) ),
	"test F01"	~: "incrementDim x User/x.x.x"				~: "User/x.x.x"												~=? ( versionToString (incrementDimension (NumberPlaceholder) $ stringToVersion "User/x.x.x" ) ),
	"test F02"	~: "incrementDim 0 Dev/x"					~: "Test/x"													~=? ( versionToString (incrementDimension (Number 0) $ stringToVersion "Dev/x" ) ),
	"test F03"	~: "incrementDim 1 Dev/1.0"					~: "Dev/2.0"												~=? ( versionToString (incrementDimension (Number 1) $ stringToVersion "Dev/1.0" ) ),
	"test F04"	~: "incrementDim 3 12.0.3"					~: "12.0.4"													~=? ( versionToString (incrementDimension (Number 3) $ stringToVersion "12.0.3" ) ),
	"test G01"	~: "decrement x"							~: "x"														~=? ( versionToString (decrement $ stringToVersion "x" ) ),
	"test G02"	~: "decrement 1"							~: "0"														~=? ( versionToString (decrement $ stringToVersion "1" ) ),
	"test G03"	~: "decrement 13"							~: "12"														~=? ( versionToString (decrement $ stringToVersion "13" ) ),
	"test H01"	~: "decrementDim x Dev/0.x.1"				~: "Dev/0.x.1"												~=? ( versionToString (decrementDimension (NumberPlaceholder) $ stringToVersion "Dev/0.x.1" ) ),
	"test H02"	~: "decrementDim 0 Test/1.x.2"				~: "Dev/1.x.2"												~=? ( versionToString (decrementDimension (Number 0) $ stringToVersion "Dev/1.x.2" ) ),
	"test H03"	~: "decrementDim 2 User/1.3.2"				~: "User/1.2.2"												~=? ( versionToString (decrementDimension (Number 2) $ stringToVersion "User/1.3.2" ) ),
	"test I01"	~: "x < 1"					~: assertError 	   "Cannot compare number placeholders and numbers" 			( stringToVersion "x" < stringToVersion "1" ),
	"test I02"	~: "3 > x"					~: assertError 	   "Cannot compare numbers and number placeholders" 			( stringToVersion "3" > stringToVersion "x" ),
	"test I03"	~: "3 > 4"									~: False 													~=? ( stringToVersion "3" > stringToVersion "4" ),
	"test I04"	~: "5 > 4"									~: True 													~=? ( stringToVersion "5" > stringToVersion "4" ),
	"test I05"	~: "Dev/1.x.3 < Prod/1.0.9"	~: assertError	   "Cannot compare number placeholders and numbers" 			( stringToVersion "Dev/1.x.3" > stringToVersion "Prod/1.0.9" ),
	"test I06"	~: "Dev/1.3.7 < Prod/1.3.7"					~: True 													~=? ( stringToVersion "Dev/1.3.7" < stringToVersion "Prod/1.3.7" ),
	"test J01"	~: "Test/1.3.4 == Test/1.3.4"				~: True 													~=? ( stringToVersion "Test/1.3.4" == stringToVersion "Test/1.3.4" ),
	"test J02"	~: "Prod/4 == Prod/4"						~: True 													~=? ( stringToVersion "Prod/4" == stringToVersion "Prod/4" ),
	"test J03"	~: "3 == 3"									~: True 													~=? ( stringToVersion "3" == stringToVersion "3" ),
	"test J04"	~: "1.0 == 1.1"								~: False 													~=? ( stringToVersion "1.0" == stringToVersion "1.1" ),
	"test _"	~: "empty test"								~: True														~=? True
	]

runTests :: IO Counts
runTests = do
	runTestTT tests