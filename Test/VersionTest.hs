module VersionTest where 

import Test.HUnit
import VersionNumber
import MaturityLevel
import Version
import Util
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Test.AssertError

versionTests = test [ 
    {-"test Z01"    ~: "generateNewVersion Dev/1.x.0"            ~: "Dev/1.x.1"                                                ~=? ( toString ( generateNewVersion $ stringToVersion "Dev/1.x.0" ) ),-}
    {-"test Z02"    ~: "generateNewVersion Prod/1.3"            ~: "Prod/1.4"                                                ~=? ( toString ( generateNewVersion $ stringToVersion "Prod/1.3" ) ),-}
    {-"test Y01"    ~: "initialVersion 1"                        ~: "x"                                                        ~=? ( toString ( initialVersion (Number 1)) ),-}
    {-"test Y02"    ~: "initialVersion 2"                        ~: "x.x"                                                    ~=? ( toString ( initialVersion (Number 2)) ),-}
    "test Y03"    ~: "isInitial x.x"                            ~: True                                                        ~=? ( isInitial (stringToVersion "x.x") ),
    "test Y04"    ~: "isInitial x.1"                            ~: False                                                    ~=? ( isInitial (stringToVersion "x.1") ),
    "test Y05"    ~: "isInitial Dev/x.x"                        ~: True                                                        ~=? ( isInitial (stringToVersion "Dev/x.x") ),
    "test Y06"    ~: "isInitial Dev/x.1"                        ~: False                                                    ~=? ( isInitial (stringToVersion "Dev/x.1") ),
    "test A01"    ~: "toString x"                        ~: "x"                                                        ~=? ( toString $ Version $ VersionNumber [Nothing] ),
    "test A02"    ~: "toString x.x.x"                    ~: "x.x.x"                                                    ~=? ( toString $ Version $ VersionNumber [Nothing, Nothing, Nothing] ),
    "test A03"    ~: "toString Dev/x.x.x"                ~: "Dev/x.x.x"                                                ~=? ( toString $ MaturityVersion Dev $ VersionNumber [Nothing, Nothing, Nothing] ),
    "test A04"    ~: "toString Prod/1.0.3"               ~: "Prod/1.0.3"                                                ~=? ( toString $ MaturityVersion Prod $ VersionNumber [Just 1, Just 0, Just 3] ),
    {-"test B01"    ~: "JSON.toJSON x"                            ~: "{\"version\":\"x\"}"                                    ~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON ( Version ( VersionNumber [Nothing] ) ) ),-}
    {-"test C01"    ~: "JSON.FromJSON Test/1.0.x"                ~: MaturityVersion Test ( VersionNumber (Number 1) (VersionNumber (Number 0) (VersionCompound (NumberPlaceholder ) ) ) )    ~=? ( parseVersionFromJSON "{\"version\": \"Test/1.0.x\"}" ),-}
    {-"test C02"    ~: "JSON.FromJSON 2.x"                        ~: (Version $ VersionNumber [Just 2, Nothing] )    ~=? ( parseVersionFromJSON "{\"version\": \"2.x\"}" ),-}
    -- "test C02"    ~: "JSON.FromJSON x"                        ~: "x"                                                        ~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON ( Version ( VersionCompound NumberPlaceholder ) ) ),
    "test D01"    ~: "stringToVersion x"                        ~: (Version $ VersionNumber [Nothing])                        ~=? ( stringToVersion "x" ),
    "test D02"    ~: "stringToVersion 1"                        ~: (Version $ VersionNumber [Just 1])                            ~=? ( stringToVersion "1" ),
    "test D03"    ~: "stringToVersion 12"                       ~: (Version $ VersionNumber [Just 12])                         ~=? ( stringToVersion "12" ),
    "test D04"    ~: "stringToVersion Dev/12"                   ~: (MaturityVersion Dev $ VersionNumber [Just 12])            ~=? ( stringToVersion "Dev/12" ),
    "test E01"    ~: "increment x"                              ~: "x"                                                        ~=? ( toString (increment $ stringToVersion "x" ) ),
    "test E02"    ~: "increment 1"                              ~: "2"                                                        ~=? ( toString (increment $ stringToVersion "1" ) ),
    "test E03"    ~: "increment 13"                             ~: "14"                                                        ~=? ( toString (increment $ stringToVersion "13" ) ),
    {-"test F01"    ~: "incrementDim x User/x.x.x"                ~: "User/x.x.x"                                                ~=? ( toString (incrementDimension (NumberPlaceholder) $ stringToVersion "User/x.x.x" ) ),-}
    "test F02"    ~: "incrementDim 0 Dev/x"                     ~: "Test/x"                                                    ~=? ( toString (incrementDimension 0 $ stringToVersion "Dev/x" ) ),
    "test F03"    ~: "incrementDim 1 Dev/1.0"                   ~: "Dev/1.1"                                                ~=? ( toString (incrementDimension 1 $ stringToVersion "Dev/1.0" ) ),
    "test F04"    ~: "incrementDim 3 12.0.3"                    ~: "13.0.3"                                                    ~=? ( toString (incrementDimension 3 $ stringToVersion "12.0.3" ) ),
    "test G01"    ~: "decrement x"                              ~: "x"                                                        ~=? ( toString (decrement $ stringToVersion "x" ) ),
    "test G02"    ~: "decrement 1"                              ~: "0"                                                        ~=? ( toString (decrement $ stringToVersion "1" ) ),
    "test G03"    ~: "decrement 13"                             ~: "12"                                                        ~=? ( toString (decrement $ stringToVersion "13" ) ),
    {-"test H01"    ~: "decrementDim x Dev/0.x.1"                 ~: "Dev/0.x.1"                                                ~=? ( toString (decrementDimension (NumberPlaceholder) $ stringToVersion "Dev/0.x.1" ) ),-}
    "test H02"    ~: "decrementDim 0 Test/1.x.2"                ~: "Dev/1.x.2"                                                ~=? ( toString (decrementDimension 0 $ stringToVersion "Dev/1.x.2" ) ),
    "test H03"    ~: "decrementDim 2 User/1.3.2"                ~: "User/1.2.2"                                                ~=? ( toString (decrementDimension 2 $ stringToVersion "User/1.3.2" ) ),
    {-"test I01"    ~: "x < 1"                    ~: assertError       "Cannot compare number placeholders and numbers"                ( stringToVersion "x" < stringToVersion "1" ),-}
    {-"test I02"    ~: "3 > x"                    ~: assertError       "Cannot compare numbers and number placeholders"                ( stringToVersion "3" > stringToVersion "x" ),-}
    "test I01"    ~: "x < 1"                                    ~: True                                                        ~=? ( stringToVersion "x" < stringToVersion "1" ),
    "test I02"    ~: "3 > x"                                    ~: True                                                        ~=? ( stringToVersion "3" > stringToVersion "x" ),
    "test I03"    ~: "3 > 4"                                    ~: False                                                    ~=? ( stringToVersion "3" > stringToVersion "4" ),
    "test I04"    ~: "5 > 4"                                    ~: True                                                        ~=? ( stringToVersion "5" > stringToVersion "4" ),
    {-"test I05"    ~: "Dev/1.x.3 < Prod/1.0.9"    ~: assertError       "Cannot compare number placeholders and numbers"                ( stringToVersion "Dev/1.x.3" > stringToVersion "Prod/1.0.9" ),-}
    "test I05"    ~: "Dev/1.x.3 < Prod/1.0.9"                    ~: True                                                        ~=? ( stringToVersion "Dev/1.x.3" < stringToVersion "Prod/1.0.9" ),
    "test I06"    ~: "Dev/1.3.7 < Prod/1.3.7"                    ~: True                                                        ~=? ( stringToVersion "Dev/1.3.7" < stringToVersion "Prod/1.3.7" ),
    "test I07"    ~: "1.3.7 < Test/1.3.7"                        ~: True                                                        ~=? ( stringToVersion "1.3.7" < stringToVersion "Test/1.3.7" ),
    "test I07"    ~: "User/1.3.7 > Test/1.3.7"                   ~: True                                                        ~=? ( stringToVersion "User/1.3.7" > stringToVersion "Test/1.3.7" ),
    "test J01"    ~: "Test/1.3.4 == Test/1.3.4"                ~: True                                                        ~=? ( stringToVersion "Test/1.3.4" == stringToVersion "Test/1.3.4" ),
    "test J02"    ~: "Prod/4 == Prod/4"                        ~: True                                                        ~=? ( stringToVersion "Prod/4" == stringToVersion "Prod/4" ),
    "test J03"    ~: "3 == 3"                                    ~: True                                                        ~=? ( stringToVersion "3" == stringToVersion "3" ),
    "test J04"    ~: "1.0 == 1.1"                                ~: False                                                    ~=? ( stringToVersion "1.0" == stringToVersion "1.1" ),
    "test J05"    ~: "1.0 == Dev/1.0"                            ~: True                                                        ~=? ( stringToVersion "1.0" == stringToVersion "Dev/1.0" ),
    "test J06"    ~: "1.0 == User/1.0"                           ~: False                                                       ~=? ( stringToVersion "1.0" == stringToVersion "User/1.0" ),
    "test K01"    ~: "promoteVersion 1.0"                        ~: (MaturityVersion Test $ VersionNumber [Just 1, Just 1])       ~=? ( promoteVersion $ stringToVersion "1.0") ,
    "test K02"    ~: "promoteVersion Test/1.0.0"                 ~: (MaturityVersion User $ VersionNumber [Just 1, Just 0, Just 1]) ~=? ( promoteVersion $ stringToVersion "Test/1.0.0") ,
    "test K03"    ~: "promoteVersion Prod/1.0.10"                ~: (MaturityVersion Prod $ VersionNumber [Just 1, Just 0, Just 11]) ~=? ( promoteVersion $ stringToVersion "Prod/1.0.10") ,
    "test _"    ~: "empty test"                                ~: True                                                        ~=? True
    ]

runTests :: IO Counts
runTests = do
    runTestTT versionTests
