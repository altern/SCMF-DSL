module VersionNumberTest where 

import Control.Exception
import Control.Monad
import Test.HUnit
import VersionNumber
import Test.AssertError

versionNumberTests = test [ 
    "test A01"  ~: "versionCompoundToString x"          ~: "x"      ~=? ( versionCompoundToString Nothing ),
    "test A02"  ~: "versionCompoundToString 1"          ~: "1"      ~=? ( versionCompoundToString ( Just 1 ) ),
    "test A03"  ~: "versionCompoundToString 2"          ~: "2"      ~=? ( versionCompoundToString ( Just 2 ) ),
    "test A04"  ~: "versionCompoundToString 1345"       ~: "1345"   ~=? ( versionCompoundToString ( Just 1345 ) ),
    "test A05"  ~: "versionNumberToString x.5"          ~: "x.5"    ~=? ( versionNumberToString   (VersionNumber [Nothing, Just 5]) ),
{-    "test B01"  ~: "decrement VC 0"        ~: "0"      ~=? ( versionCompoundToString ( decrement (Just 0 ) ) ),-}
    {-"test B02"  ~: "decrement VC 1"        ~: "0"      ~=? ( versionCompoundToString ( decrement (Just 1 ) ) ),-}
    {-"test B03"  ~: "decrement VC 256"      ~: "255"    ~=? ( versionCompoundToString ( decrement (Just 256 ) ) ),-}
    {-"test B04"  ~: "decrement VC x"        ~: "x"      ~=? ( versionCompoundToString ( decrement (Nothing) ) ),-}
    {-"test C01"  ~: "increment VC 0"        ~: "1"      ~=? ( versionCompoundToString ( increment (Just 0 ) ) ),-}
    {-"test C02"  ~: "increment VC 1"        ~: "2"      ~=? ( versionCompoundToString ( increment (Just 1 ) ) ),-}
    {-"test C03"  ~: "increment VC 256"      ~: "257"    ~=? ( versionCompoundToString ( increment (Just 256 ) ) ),-}
    {-"test C04"  ~: "increment VC x"        ~: "x"      ~=? ( versionCompoundToString ( increment (Nothing) ) ), -}
    {-"test E01"  ~: "decrement VN x"          ~: "x"      ~=? ( versionNumberToString ( decrement (VC Nothing) ) ),-}
    {-"test E02"  ~: "decrement VN 3"          ~: "3"      ~=? ( versionNumberToString ( decrement (VC (Just 4) ) ) ),-}
    {-"test E03"  ~: "decrement VN 1"          ~: "0"      ~=? ( versionNumberToString ( decrement (VC (Just 1) ) ) ),-}
    {-"test E04"  ~: "decrement VN 0"          ~: "0"      ~=? ( versionNumberToString ( decrement (VC (Just 0) ) ) ),-}
    {-"test F01"  ~: "increment VN x"          ~: "x"      ~=? ( versionNumberToString ( increment (VC Nothing) ) ),-}
    {-"test F02"  ~: "increment VN 4"          ~: "5"      ~=? ( versionNumberToString ( increment (VC (Just 4) ) ) ),-}
    {-"test F03"  ~: "increment VN 35"         ~: "36"     ~=? ( versionNumberToString ( increment (VC (Just 35) ) ) ),-}
    {-"test G01"  ~: "decrementDim 0 VN 5"     ~: "5"      ~=? ( versionNumberToString ( decrementDimension (Just 0) ( VC (Just 5) ) ) ),-}
    {-"test G02"  ~: "decrementDim 1 VN 5"     ~: "4"      ~=? ( versionNumberToString ( decrementDimension (Just 1) ( VC (Just 5) ) ) ),-}
    {-"test G03"  ~: "decrementDim 0 VN x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Just 0) (VN (VC Nothing) Nothing ) ) ),-}
    {-"test G04"  ~: "decrementDim 1 VN x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Just 1) (VN (VC Nothing) Nothing ) ) ),-}
    {-"test G05"  ~: "decrementDim 2 VN x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Just 2) (VN (VC Nothing) Nothing ) ) ),-}
    {-"test G06"  ~: "decrementDim 3 VN x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Just 3) (VN (VC Nothing) Nothing ) ) ),-}
    {-"test G07"  ~: "decrementDim 0 VN 3.x"   ~: "3.x"    ~=? ( versionNumberToString ( decrementDimension (Just 0) (VN (VC (Just 3)) Nothing) ) ),-}
    {-"test G08"  ~: "decrementDim 1 VN 3.1"   ~: "3.0"    ~=? ( versionNumberToString ( decrementDimension (Just 1) (VN (VC (Just 3)) (Just 1)) ) ),-}
    {-"test G09"  ~: "decrementDim 2 VN x.5"   ~: "x.5"    ~=? ( versionNumberToString ( decrementDimension (Just 2) (VN (VC Nothing) (Just 5)) ) ),-}
    {-"test G10"  ~: "decrementDim 3 VN x.6"   ~: "x.6"    ~=? ( versionNumberToString ( decrementDimension (Just 3) (VN (VC Nothing) (Just 6)) ) ),-}
    {-"test G10"  ~: "decrementDim x VN x.6"   ~: "x.6"    ~=? ( versionNumberToString ( decrementDimension Nothing (VN (VC Nothing) (Just 6)) ) ),-}
    {-"test G11"  ~: "decrementDim 3 VN x.x.6" ~: "x.x.6"  ~=? ( versionNumberToString ( decrementDimension (Just 3) (VN (VN ( VC Nothing ) Nothing ) (Just 6) ) ) ),-}
    {-"test H01"  ~: "incrementDim 0 VN x.1"   ~: "x.1"    ~=? ( versionNumberToString ( incrementDimension (Just 0) (VN (VC Nothing) (Just 1)) ) ),-}
    {-"test H02"  ~: "incrementDim 1 VN x.2"   ~: "x.3"    ~=? ( versionNumberToString ( incrementDimension (Just 1) (VN (VC Nothing) (Just 2)) ) ),-}
    {-"test H03"  ~: "incrementDim 1 VN 3.2"   ~: "3.3"    ~=? ( versionNumberToString ( incrementDimension (Just 1) (VN (VC (Just 3)) (Just 2)) ) ),-}
    {-"test H04"  ~: "incrementDim 2 VN 3.2"   ~: "4.2"    ~=? ( versionNumberToString ( incrementDimension (Just 2) (VN (VC (Just 3)) (Just 2)) ) ),-}
    {-"test H05"  ~: "incrementDim 3 VN 3.2"   ~: "3.2"    ~=? ( versionNumberToString ( incrementDimension (Just 3) (VN (VC (Just 3)) (Just 2)) ) ),-}
    {-"test H06"  ~: "incrementDim x VN 2.0"   ~: "2.0"    ~=? ( versionNumberToString ( incrementDimension Nothing (VN (VC (Just 2)) (Just 0)) ) ),-}
    {-"test H07"  ~: "incrementDim 3 VN x.x.1" ~: "x.x.1"  ~=? ( versionNumberToString ( incrementDimension (Just 3) (VN (VN (VC Nothing) Nothing) (Just 1) ) ) ),-}
    {-"test I01"  ~: "createVNByNumberOfDimensions x"        ~: "x"        ~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions Nothing ),-}
    {-"test I02"  ~: "createVNByNumberOfDimensions 0"        ~: "x"        ~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Just 0) ),-}
    {-"test I03"  ~: "createVNByNumberOfDimensions 1"        ~: "x"        ~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Just 1) ),-}
    {-"test I04"  ~: "createVNByNumberOfDimensions 2"        ~: "x.x"    ~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Just 2) ),-}
    {-"test I05"  ~: "createVNByNumberOfDimensions 4"        ~: "x.x.x.x"~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Just 4) ),-}
    {-"test J01"  ~: "isInitial x"                           ~: True        ~=? ( isInitial (VC Nothing) ),-}
    {-"test J02"  ~: "isInitial x.x"                         ~: True        ~=? ( isInitial (VN (VC Nothing) Nothing ) ),-}
    {-"test J03"  ~: "isInitial 1"                          ~: False    ~=? ( isInitial (VC (Just 1) ) ),-}
    {-"test J03a" ~: "isInitial 0"                          ~: False    ~=? ( isInitial (VC (Just 0) ) ),-}
    {-"test J03b" ~: "isInitial x.0"                        ~: False    ~=? ( isInitial $ stringToVersionNumber "x.0"),-}
    {-"test J04"  ~: "isInitial 5"                          ~: False    ~=? ( isInitial (VC (Just 5) ) ),-}
    "test L01"  ~: "stringToVersionNumber x"              ~: ( stringToVersionNumber "x" )        ~=? (VersionNumber [Nothing]),
    "test L02"  ~: "stringToVersionNumber x.x"            ~: ( stringToVersionNumber "x.x" )        ~=? (VersionNumber [Nothing, Nothing]),
    "test L03"  ~: "stringToVersionNumber x.5"            ~: ( stringToVersionNumber "x.5" )        ~=? (VersionNumber [Nothing, Just 5]),
    "test L04"  ~: "stringToVersionNumber 1.x.5"          ~: ( stringToVersionNumber "1.x.5" )    ~=? (VersionNumber [Just 1, Nothing, Just 5]),
    {-"test M01"  ~: "x == x"                                ~: True        ~=? Nothing == Nothing,-}
    "test M02"  ~: "1 == 1"                               ~: True        ~=? (Just 1) == (Just 1),
    "test M03"  ~: "5 == 5"                               ~: True        ~=? (Just 5) == (Just 5),
    "test M04"  ~: "3 == 5"                               ~: False    ~=? (Just 3) == (Just 5),
    "test M05"  ~: "27 == x"                              ~: False    ~=? (Just 27) == (Nothing),
    "test M06"  ~: "x.x.3 == x.x.x.x.x.3"                 ~: True        ~=? ( stringToVersionNumber "x.x.3" == stringToVersionNumber "x.x.x.x.x.3" ),
    "test M07"  ~: "26 == x.x.x.26"                       ~: True        ~=? ( stringToVersionNumber "26" == stringToVersionNumber "x.x.x.26" ),
    "test M07"  ~: "x.x.x.33.x == 33.x"                   ~: True        ~=? ( stringToVersionNumber "x.x.x.33.x" == stringToVersionNumber "33.x" ),
    "test M08"  ~: "x.x.1.x.0.x == 1.x.0.x"               ~: True        ~=? ( stringToVersionNumber "x.x.1.x.0.x" == stringToVersionNumber "1.x.0.x" ),
    "test N01"  ~: "1.x.x < 2.x.x"                        ~: True        ~=? ( stringToVersionNumber "1.x.x" < stringToVersionNumber "2.x.x" ) ,
    "test N02"  ~: "x.5 > x.3"                            ~: True        ~=? ( stringToVersionNumber "x.5" > stringToVersionNumber "x.3" ) ,
    "test N03"  ~: "1.0 < 1.1"                            ~: True        ~=? ( stringToVersionNumber "1.0" < stringToVersionNumber "1.1" ) ,
    "test N04"  ~: "2.0.6 < 2.1.5"                        ~: True        ~=? ( stringToVersionNumber "2.0.6" < stringToVersionNumber "2.1.5" ) ,
    "test N05"  ~: "2.x.x > 1.5"                          ~: True        ~=? ( stringToVersionNumber "2.x.x" > stringToVersionNumber "1.5" ) ,
    "test N06"  ~: "2.x.x > x.1.5"                        ~: True        ~=? ( stringToVersionNumber "2.x.x" > stringToVersionNumber "x.1.5" ) ,
    "test N07"  ~: "2.0.0 > 2.0.0.x"                      ~: False    ~=? ( stringToVersionNumber "2.0.0" > stringToVersionNumber "2.0.0.x" ) ,
    "test N08"  ~: "2.0.0 > 2.0.0.x.x"                    ~: False    ~=? ( stringToVersionNumber "2.0.0" > stringToVersionNumber "2.0.0.x.x" ) ,
    "test N09"  ~: "3.x.x > 3.x.x.x.x"                    ~: False    ~=? ( stringToVersionNumber "3.x.x" > stringToVersionNumber "3.x.x.x.x" ) ,
    "test N10"  ~: "3 > x"                                ~: True     ~=? ( stringToVersionNumber "3" > stringToVersionNumber "x" ) ,
    "test N11"  ~: "x > 4"                                ~: False    ~=? ( stringToVersionNumber "x" > stringToVersionNumber "4" ) ,
    "test N12"  ~: "2.x.x > 2.5.1"                        ~: False    ~=? ( stringToVersionNumber "2.x.x" > stringToVersionNumber "2.5.1" ) ,
    "test N13"  ~: "2 > x.1"                              ~: True     ~=? ( stringToVersionNumber "2" > stringToVersionNumber "x.1" ) ,
    "test N14"  ~: "1.x > 1"                              ~: True     ~=? ( stringToVersionNumber "1.x" > stringToVersionNumber "1" ) ,
    {-"test N09"  ~: assertError "Cannot compare numbers and number placeholders" ( (Just 3) > (Nothing) ) ,-}
    {-"test N10"  ~: assertError "Cannot compare number placeholders and numbers" ( (Nothing) > (Just 4) ) ,-}
    {-"test N11"  ~: assertError "Cannot compare numbers and number placeholders" ( (Just 3) < (Nothing) ) ,-}
    {-"test N12"  ~: assertError "Cannot compare number placeholders and numbers" ( (Nothing) < (Just 4) ) ,-}
    {-"test N13"  ~: assertError "Cannot compare number placeholders and numbers" ( stringToVersionNumber "2.x.x" < stringToVersionNumber "2.5.1" ) ,-}
{-    "test Q01"  ~: "getNumberOfDimensions x"            ~: (Just 1) ~=? ( getNumberOfDimensions (stringToVersionNumber "x") ) ,-}
    {-"test Q02"  ~: "getNumberOfDimensions x.4"          ~: (Just 2) ~=? ( getNumberOfDimensions (stringToVersionNumber "x.4") ) ,-}
    {-"test Q03"  ~: "getNumberOfDimensions x.4.x"        ~: (Just 3) ~=? ( getNumberOfDimensions (stringToVersionNumber "x.4.x") ) ,-}
    {-"test R01"  ~: "appendDimension x"                  ~: "x.x"    ~=? ( versionNumberToString $ appendDimension (stringToVersionNumber "x") ) ,-}
    {-"test R02"  ~: "appendDimension x.x"                ~: "x.x.x"  ~=? ( versionNumberToString $ appendDimension (stringToVersionNumber "x.x") ) ,-}
    {-"test R03"  ~: "appendDimension 1"                  ~: "x.1"    ~=? ( versionNumberToString $ appendDimension (stringToVersionNumber "1") ) ,-}
    {-"test S01"  ~: "isReleaseBranch 1.x"               ~: True     ~=? ( isReleaseBranch $ stringToVersionNumber "1.x" ) ,-}
    {-"test S02"  ~: "isReleaseBranch 2.x"               ~: True     ~=? ( isReleaseBranch $ stringToVersionNumber "2.x" ) ,-}
    {-"test S03"  ~: "isReleaseBranch 2.x.x"             ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "2.x.x" ) ,-}
    {-"test S04"  ~: "isReleaseBranch 2.0.x"             ~: True     ~=? ( isReleaseBranch $ stringToVersionNumber "2.0.x" ) ,-}
    {-"test S05"  ~: "isReleaseBranch x.0.x"             ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "x.0.x" ) ,-}
    {-"test S06"  ~: "isReleaseBranch x.1.x"             ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "x.1.x" ) ,-}
    {-"test S07"  ~: "isReleaseBranch x.x.1.x"           ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "x.x.1.x" ) ,-}
    {-"test S08"  ~: "isReleaseBranch x.1.x.x"           ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "x.1.x.x" ) ,-}
    {-"test S09"  ~: "isReleaseBranch 1.x.x"             ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "1.x.x" ) ,-}
    {-"test S10"  ~: "isReleaseBranch 1"                 ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "1" ) ,-}
    {-"test S11"  ~: "isReleaseBranch x.1"               ~: False    ~=? ( isReleaseBranch $ stringToVersionNumber "x.1" ) ,-}
    {-"test T01"  ~: "isSupportBranch 1.x.x"             ~: True     ~=? ( isSupportBranch $ stringToVersionNumber "1.x.x" ) ,-}
    {-"test T02"  ~: "isSupportBranch 0.x.x"             ~: True     ~=? ( isSupportBranch $ stringToVersionNumber "0.x.x" ) ,-}
    {-"test T03"  ~: "isSupportBranch 2.x.x"             ~: True     ~=? ( isSupportBranch $ stringToVersionNumber "2.x.x" ) ,-}
    {-"test T04"  ~: "isSupportBranch 2.x.0"             ~: False    ~=? ( isSupportBranch $ stringToVersionNumber "2.x.0" ) ,-}
    {-"test T05"  ~: "isSupportBranch 2.0.9"             ~: False    ~=? ( isSupportBranch $ stringToVersionNumber "2.0.9" ) ,-}
    {-"test T06"  ~: "isSupportBranch x.0.x.x"           ~: False    ~=? ( isSupportBranch $ stringToVersionNumber "x.0.x.x" ) ,-}
    {-"test T07"  ~: "isSupportBranch x.x.x.x"           ~: False    ~=? ( isSupportBranch $ stringToVersionNumber "x.x.x.x" ) ,-}
    {-"test T08"  ~: "isSupportBranch x.x"               ~: False    ~=? ( isSupportBranch $ stringToVersionNumber "x.x" ) ,-}
    {-"test U01"  ~: "isReleaseSnapshot x.x"             ~: False    ~=? ( isReleaseSnapshot $ stringToVersionNumber "x.x" ) ,-}
    {-"test U02"  ~: "isReleaseSnapshot 1.x"             ~: False    ~=? ( isReleaseSnapshot $ stringToVersionNumber "1.x" ) ,-}
    {-"test U03"  ~: "isReleaseSnapshot 1.0"             ~: True     ~=? ( isReleaseSnapshot $ stringToVersionNumber "1.0" ) ,-}
    {-"test U04"  ~: "isReleaseSnapshot x.1.0"           ~: False    ~=? ( isReleaseSnapshot $ stringToVersionNumber "x.1.0" ) ,-}
    {-"test U05"  ~: "isReleaseSnapshot 0.1.0"           ~: True     ~=? ( isReleaseSnapshot $ stringToVersionNumber "0.1.0" ) ,-}
    {-"test V01"  ~: "isSupportSnapshot x.x"             ~: False    ~=? ( isSupportSnapshot $ stringToVersionNumber "x.x" ) ,-}
    {-"test V02"  ~: "isSupportSnapshot 1.x"             ~: False    ~=? ( isSupportSnapshot $ stringToVersionNumber "1.x" ) ,-}
    {-"test V03"  ~: "isSupportSnapshot 1.0"             ~: False    ~=? ( isSupportSnapshot $ stringToVersionNumber "1.0" ) ,-}
    {-"test V04"  ~: "isSupportSnapshot x.1.0"           ~: False    ~=? ( isSupportSnapshot $ stringToVersionNumber "x.1.0" ) ,-}
    {-"test V05"  ~: "isSupportSnapshot 0.1.0"           ~: False    ~=? ( isSupportSnapshot $ stringToVersionNumber "0.1.0" ) ,-}
    {-"test V06"  ~: "isSupportSnapshot 0.x.0"           ~: True     ~=? ( isSupportSnapshot $ stringToVersionNumber "0.x.0" ) ,-}
    {-"test V07"  ~: "isSupportSnapshot x.0.x.0"         ~: False    ~=? ( isSupportSnapshot $ stringToVersionNumber "x.0.x.0" ) ,-}
    {-"test W01"  ~: "isExperimentalBranch x.x.x.x"      ~: True     ~=? ( isExperimentalBranch $ stringToVersionNumber "x.x.x.x" ) ,-}
    {-"test W02"  ~: "isExperimentalBranch x"            ~: True     ~=? ( isExperimentalBranch $ stringToVersionNumber "x" ) ,-}
    {-"test W03"  ~: "isExperimentalBranch 1.x"          ~: False    ~=? ( isExperimentalBranch $ stringToVersionNumber "1.x" ) ,-}
    "test _"    ~: "empty test"                        ~: True     ~=? True
    ]

runTests :: IO Counts
runTests = do
    runTestTT versionNumberTests
