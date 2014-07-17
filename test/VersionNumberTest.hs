module VersionNumberTest where 

import Control.Exception
import Control.Monad
import Test.HUnit
import VersionNumber

instance Eq ErrorCall where
    x == y = (show x) == (show y)

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex) 

assertError ex f = 
    TestCase $ assertException (ErrorCall ex) $ evaluate f

tests = test [ 
    "test A01"  ~: "versionCompoundToString x"          ~: "x"      ~=? ( versionCompoundToString NumberPlaceholder ),
    "test A02"  ~: "versionCompoundToString 1"          ~: "1"      ~=? ( versionCompoundToString ( Number 1 ) ),
    "test A03"  ~: "versionCompoundToString 2"          ~: "2"      ~=? ( versionCompoundToString ( Number 2 ) ),
    "test A04"  ~: "versionCompoundToString 1345"       ~: "1345"   ~=? ( versionCompoundToString ( Number 1345 ) ),
    "test B01"  ~: "decrement VersionCompound 0"        ~: "0"      ~=? ( versionCompoundToString ( decrement (Number 0 ) ) ),
    "test B02"  ~: "decrement VersionCompound 1"        ~: "0"      ~=? ( versionCompoundToString ( decrement (Number 1 ) ) ),
    "test B03"  ~: "decrement VersionCompound 256"      ~: "255"    ~=? ( versionCompoundToString ( decrement (Number 256 ) ) ),
    "test B04"  ~: "decrement VersionCompound x"        ~: "x"      ~=? ( versionCompoundToString ( decrement (NumberPlaceholder) ) ),
    "test C01"  ~: "increment VersionCompound 0"        ~: "1"      ~=? ( versionCompoundToString ( increment (Number 0 ) ) ),
    "test C02"  ~: "increment VersionCompound 1"        ~: "2"      ~=? ( versionCompoundToString ( increment (Number 1 ) ) ),
    "test C03"  ~: "increment VersionCompound 256"      ~: "257"    ~=? ( versionCompoundToString ( increment (Number 256 ) ) ),
    "test C04"  ~: "increment VersionCompound x"        ~: "x"      ~=? ( versionCompoundToString ( increment (NumberPlaceholder) ) ), 
    "test D01"  ~: "generateNewVersionCompound x"       ~: "x"      ~=? ( versionCompoundToString $ generateNewVersionCompound (NumberPlaceholder) ), 
    "test D02"  ~: "generateNewVersionCompound 0"       ~: "1"      ~=? ( versionCompoundToString $ generateNewVersionCompound (Number 0) ), 
    "test D03"  ~: "generateNewVersionCompound 1"       ~: "2"      ~=? ( versionCompoundToString $ generateNewVersionCompound (Number 1) ), 
    "test D04"  ~: "generateNewVersionCompound 25"      ~: "26"     ~=? ( versionCompoundToString $ generateNewVersionCompound (Number 25) ), 
    "test E01"  ~: "decrement VersionNumber x"          ~: "x"      ~=? ( versionNumberToString ( decrement (VersionCompound NumberPlaceholder) ) ),
    "test E02"  ~: "decrement VersionNumber 3"          ~: "3"      ~=? ( versionNumberToString ( decrement (VersionCompound (Number 4) ) ) ),
    "test E03"  ~: "decrement VersionNumber 1"          ~: "0"      ~=? ( versionNumberToString ( decrement (VersionCompound (Number 1) ) ) ),
    "test E04"  ~: "decrement VersionNumber 0"          ~: "0"      ~=? ( versionNumberToString ( decrement (VersionCompound (Number 0) ) ) ),
    "test F01"  ~: "increment VersionNumber x"          ~: "x"      ~=? ( versionNumberToString ( increment (VersionCompound NumberPlaceholder) ) ),
    "test F02"  ~: "increment VersionNumber 4"          ~: "5"      ~=? ( versionNumberToString ( increment (VersionCompound (Number 4) ) ) ),
    "test F03"  ~: "increment VersionNumber 35"         ~: "36"     ~=? ( versionNumberToString ( increment (VersionCompound (Number 35) ) ) ),
    "test G01"  ~: "decrementDim 0 VersionNumber 5"     ~: "5"      ~=? ( versionNumberToString ( decrementDimension (Number 0) ( VersionCompound (Number 5) ) ) ),
    "test G02"  ~: "decrementDim 1 VersionNumber 5"     ~: "4"      ~=? ( versionNumberToString ( decrementDimension (Number 1) ( VersionCompound (Number 5) ) ) ),
    "test G03"  ~: "decrementDim 0 VersionNumber x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Number 0) (VersionNumber NumberPlaceholder ( VersionCompound NumberPlaceholder) ) ) ),
    "test G04"  ~: "decrementDim 1 VersionNumber x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Number 1) (VersionNumber NumberPlaceholder ( VersionCompound NumberPlaceholder) ) ) ),
    "test G05"  ~: "decrementDim 2 VersionNumber x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Number 2) (VersionNumber NumberPlaceholder ( VersionCompound NumberPlaceholder) ) ) ),
    "test G06"  ~: "decrementDim 3 VersionNumber x.x"   ~: "x.x"    ~=? ( versionNumberToString ( decrementDimension (Number 3) (VersionNumber NumberPlaceholder ( VersionCompound NumberPlaceholder) ) ) ),
    "test G07"  ~: "decrementDim 0 VersionNumber 3.x"   ~: "3.x"    ~=? ( versionNumberToString ( decrementDimension (Number 0) (VersionNumber (Number 3) ( VersionCompound NumberPlaceholder ) ) ) ),
    "test G08"  ~: "decrementDim 1 VersionNumber 3.1"   ~: "2.1"    ~=? ( versionNumberToString ( decrementDimension (Number 1) (VersionNumber (Number 3) ( VersionCompound (Number 1) ) ) ) ),
    "test G09"  ~: "decrementDim 2 VersionNumber x.5"   ~: "x.4"    ~=? ( versionNumberToString ( decrementDimension (Number 2) (VersionNumber NumberPlaceholder ( VersionCompound (Number 5) ) ) ) ),
    "test G10"  ~: "decrementDim 3 VersionNumber x.6"   ~: "x.6"    ~=? ( versionNumberToString ( decrementDimension (Number 3) (VersionNumber NumberPlaceholder ( VersionCompound (Number 6) ) ) ) ),
    "test G10"  ~: "decrementDim x VersionNumber x.6"   ~: "x.6"    ~=? ( versionNumberToString ( decrementDimension NumberPlaceholder (VersionNumber NumberPlaceholder ( VersionCompound (Number 6) ) ) ) ),
    "test G11"  ~: "decrementDim 3 VersionNumber x.x.6" ~: "x.x.5"  ~=? ( versionNumberToString ( decrementDimension (Number 3) (VersionNumber NumberPlaceholder (VersionNumber NumberPlaceholder ( VersionCompound (Number 6) ) ) ) ) ),
    "test H01"  ~: "incrementDim 0 VersionNumber x.1"   ~: "x.1"    ~=? ( versionNumberToString ( incrementDimension (Number 0) (VersionNumber NumberPlaceholder ( VersionCompound (Number 1) ) ) ) ),
    "test H02"  ~: "incrementDim 1 VersionNumber x.2"   ~: "x.2"    ~=? ( versionNumberToString ( incrementDimension (Number 1) (VersionNumber NumberPlaceholder ( VersionCompound (Number 2) ) ) ) ),
    "test H03"  ~: "incrementDim 1 VersionNumber 3.2"   ~: "4.2"    ~=? ( versionNumberToString ( incrementDimension (Number 1) (VersionNumber (Number 3) ( VersionCompound (Number 2) ) ) ) ),
    "test H04"  ~: "incrementDim 2 VersionNumber 3.2"   ~: "3.3"    ~=? ( versionNumberToString ( incrementDimension (Number 2) (VersionNumber (Number 3) ( VersionCompound (Number 2) ) ) ) ),
    "test H05"  ~: "incrementDim 3 VersionNumber 3.2"   ~: "3.2"    ~=? ( versionNumberToString ( incrementDimension (Number 3) (VersionNumber (Number 3) ( VersionCompound (Number 2) ) ) ) ),
    "test H06"  ~: "incrementDim x VersionNumber 2.0"   ~: "2.0"    ~=? ( versionNumberToString ( incrementDimension NumberPlaceholder (VersionNumber (Number 2) ( VersionCompound (Number 0) ) ) ) ),
    "test H07"  ~: "incrementDim 3 VersionNumber x.x.1" ~: "x.x.2"  ~=? ( versionNumberToString ( incrementDimension (Number 3) (VersionNumber NumberPlaceholder (VersionNumber NumberPlaceholder ( VersionCompound (Number 1) ) ) ) ) ),
    "test I01"  ~: "createVNByNumberOfDimensions x"		~: "x"		~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions NumberPlaceholder ),
    "test I02"  ~: "createVNByNumberOfDimensions 0"		~: "x"		~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Number 0) ),
    "test I03"  ~: "createVNByNumberOfDimensions 1"		~: "x"		~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Number 1) ),
    "test I04"  ~: "createVNByNumberOfDimensions 2"		~: "x.x"	~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Number 2) ),
    "test I05"  ~: "createVNByNumberOfDimensions 4"		~: "x.x.x.x"~=? ( versionNumberToString $ createVersionNumberByNumberOfDimensions (Number 4) ),
    "test J01"  ~: "isInitalVersionNumber x"			~: True 	~=? ( isInitialVersionNumber (VersionCompound NumberPlaceholder) ),
    "test J02"  ~: "isInitalVersionNumber 1"			~: False 	~=? ( isInitialVersionNumber (VersionCompound (Number 1) ) ),
    "test J03"  ~: "isInitalVersionNumber 5"			~: False 	~=? ( isInitialVersionNumber (VersionCompound (Number 5) ) ),
    "test K01"  ~: "stringToVersionCompound x"			~: ( stringToVersionCompound "x" ) 		~=? NumberPlaceholder,
    "test K02"  ~: "stringToVersionCompound 0"			~: ( stringToVersionCompound "0" ) 		~=? (Number 0),
    "test K03"  ~: "stringToVersionCompound 1"			~: ( stringToVersionCompound "1" ) 		~=? (Number 1),
    "test K04"  ~: "stringToVersionCompound 9"			~: ( stringToVersionCompound "9" ) 		~=? (Number 9),
    "test L01"  ~: "stringToVersionNumber x"			~: ( stringToVersionNumber "x" ) 		~=? (VersionCompound NumberPlaceholder),
    "test L02"  ~: "stringToVersionNumber x.x"			~: ( stringToVersionNumber "x.x" ) 		~=? (VersionNumber NumberPlaceholder ( VersionCompound NumberPlaceholder) ),
    "test L03"  ~: "stringToVersionNumber x.5"			~: ( stringToVersionNumber "x.5" ) 		~=? (VersionNumber NumberPlaceholder ( VersionCompound (Number 5) ) ),
    "test L04"  ~: "stringToVersionNumber 1.x.5"		~: ( stringToVersionNumber "1.x.5" ) 	~=? (VersionNumber (Number 1) ( VersionNumber NumberPlaceholder ( VersionCompound (Number 5) ) ) ),
    "test M01"  ~: "x == x"								~: True 	~=? NumberPlaceholder == NumberPlaceholder,
    "test M02"  ~: "1 == 1"								~: True 	~=? (Number 1) == (Number 1),
    "test M03"  ~: "5 == 5"								~: True 	~=? (Number 5) == (Number 5),
    "test M04"  ~: "3 == 5"								~: False	~=? (Number 3) == (Number 5),
    "test M05"  ~: "27 == x"							~: False	~=? (Number 27) == (NumberPlaceholder),
    "test M06"  ~: "3.x.x == 3.x.x.x.x"					~: True		~=? ( stringToVersionNumber "3.x.x" == stringToVersionNumber "3.x.x.x.x" ),
    "test M07"  ~: "26 == 26.x.x.x"						~: True		~=? ( stringToVersionNumber "26" == stringToVersionNumber "26.x.x.x" ),
    "test M07"  ~: "33.x.x.x.x == 33.x"					~: True		~=? ( stringToVersionNumber "33.x.x.x.x" == stringToVersionNumber "33.x" ),
    "test N01"  ~: "1.x.x < 2.x.x" 						~: True 	~=? ( stringToVersionNumber "1.x.x" < stringToVersionNumber "2.x.x" ) ,
    "test N02"  ~: "x.5 > x.3" 							~: True 	~=? ( stringToVersionNumber "x.5" > stringToVersionNumber "x.3" ) ,
    "test N03"  ~: "1.0 < 1.1" 							~: True 	~=? ( stringToVersionNumber "1.0" < stringToVersionNumber "1.1" ) ,
    "test N04"  ~: "2.0.6 < 2.1.5" 						~: True 	~=? ( stringToVersionNumber "2.0.6" < stringToVersionNumber "2.1.5" ) ,
    "test N05"  ~: "2.x.x > 1.5" 						~: True 	~=? ( stringToVersionNumber "2.x.x" > stringToVersionNumber "1.5" ) ,
    "test N06"  ~: "2.0.0 > 2.0.0.x" 					~: False 	~=? ( stringToVersionNumber "2.0.0" > stringToVersionNumber "2.0.0.x" ) ,
    "test N07"  ~: "2.0.0 > 2.0.0.x.x" 					~: False 	~=? ( stringToVersionNumber "2.0.0" > stringToVersionNumber "2.0.0.x.x" ) ,
    "test N08"  ~: "3.x.x > 3.x.x.x.x" 					~: False 	~=? ( stringToVersionNumber "3.x.x" > stringToVersionNumber "3.x.x.x.x" ) ,
    "test N09"  ~: assertError "Cannot compare numbers and number placeholders" ( (Number 3) > (NumberPlaceholder) ) ,
    "test N10"  ~: assertError "Cannot compare number placeholders and numbers" ( (NumberPlaceholder) > (Number 4) ) ,
    "test N11"  ~: assertError "Cannot compare numbers and number placeholders" ( (Number 3) < (NumberPlaceholder) ) ,
    "test N12"  ~: assertError "Cannot compare number placeholders and numbers" ( (NumberPlaceholder) < (Number 4) ) ,
    "test N13"  ~: assertError "Cannot compare number placeholders and numbers" ( stringToVersionNumber "2.x.x" < stringToVersionNumber "2.5.1" ) ,
    "test O01"  ~: "freezeDimensionByNum 2 3.x.x" 		~: "3.0.x" 	~=? ( versionNumberToString $ freezeDimensionByNum (Number 2) (stringToVersionNumber "3.x.x") ) ,
    "test O02"  ~: "freezeDimensionByNum 4 x.x.x.x" 	~: "x.x.x.0"~=? ( versionNumberToString $ freezeDimensionByNum (Number 4) (stringToVersionNumber "x.x.x.0") ) ,
    "test O03"  ~: "freezeDimensionByNum x 1.0.x" 		~: "1.0.x"	~=? ( versionNumberToString $ freezeDimensionByNum (NumberPlaceholder) (stringToVersionNumber "1.0.x") ) ,
    "test O04"  ~: "freezeDimensionByNum 0 x.0.x" 		~: "x.0.x"	~=? ( versionNumberToString $ freezeDimensionByNum (Number 0) (stringToVersionNumber "x.0.x") ) ,
    "test O05"  ~: "freezeDimensionByNum 5 2.x.4.x" 	~: "2.x.4.x"~=? ( versionNumberToString $ freezeDimensionByNum (Number 5) (stringToVersionNumber "2.x.4.x") ) ,
    "test _"  	~: "empty test"							~: True 	~=? True
    ]

main :: IO Counts
main = do
    runTestTT tests