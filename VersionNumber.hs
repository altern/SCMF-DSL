{-# LANGUAGE OverloadedStrings #-}

module VersionNumber where

import Data.Attoparsec.Char8
-- import Data.ByteString.Char8
import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
-- import System.Directory

type NumberOfDimensions = VersionCompound

data VersionCompound = NumberPlaceholder                      -- X
                     | Number Int                           -- 1, 2, 3, ..., 45, ... 
                     deriving (Show)

{-instance Monad VersionCompound where
	return x = Number x
	NumberPlaceholder >>= f = NumberPlaceholder
	Number num >>= f = f num
	fail _ = NumberPlaceholder
	-}
generateNewVersionCompound :: VersionCompound -> VersionCompound
generateNewVersionCompound NumberPlaceholder = NumberPlaceholder
generateNewVersionCompound (Number num) = Number (num + 1)

class VersionOperations a where
    decrement :: a -> a
    decrementDimension :: NumberOfDimensions -> a -> a
    increment :: a -> a
    incrementDimension :: NumberOfDimensions -> a -> a
    getNumberOfDimensions :: a -> NumberOfDimensions
    appendDimension :: a -> a

instance VersionOperations VersionCompound where 
    decrement           NumberPlaceholder       = NumberPlaceholder
    decrement           (Number 0)              = Number 0
    decrement           (Number num)            = Number (num - 1)
    decrementDimension  _                   a   = decrement a
    increment           NumberPlaceholder       = NumberPlaceholder
    increment           (Number num)            = Number (num + 1)
    incrementDimension  _                   a   = increment a

data VersionNumber = VersionCompound VersionCompound
       | VersionNumber VersionCompound VersionNumber
       deriving (Show)

data VersionNumberWithMaybe = VC (Maybe Int)
		| VN (Maybe Int) VersionNumberWithMaybe
		deriving (Show)

generateNewVersionNumber :: VersionNumber -> VersionNumber
generateNewVersionNumber ( VersionCompound vc ) = ( VersionCompound (generateNewVersionCompound vc) )
generateNewVersionNumber ( VersionNumber vc vn ) = ( VersionNumber vc (generateNewVersionNumber vn) )
generateNewVersionNumber ( VersionNumber vc vn@(VersionCompound NumberPlaceholder) ) = ( VersionNumber (generateNewVersionCompound vc) vn )
generateNewVersionNumber ( VersionNumber vc vn@(VersionNumber _ _ ) ) = ( VersionNumber vc (generateNewVersionNumber vn ) )
generateNewVersionNumber ( VersionNumber vc vn@(VersionCompound _ ) ) = ( VersionNumber vc (generateNewVersionNumber vn ) )

instance VersionOperations VersionNumber where 
    decrement                              ( VersionCompound vc )                           = VersionCompound ( decrement vc )
    decrement                              ( VersionNumber vc vn )                          = VersionNumber vc (decrement vn)
    decrementDimension NumberPlaceholder   ( VersionCompound vc )                           = VersionCompound vc
    decrementDimension (Number 0)          ( VersionCompound vc )                           = VersionCompound vc
    decrementDimension (Number 1)          ( VersionCompound vc )                           = VersionCompound ( decrement vc )
    decrementDimension dim                 ( VersionCompound vc )                           = VersionCompound ( decrementDimension ( decrement dim ) vc )
    decrementDimension NumberPlaceholder vn@( VersionNumber _ _ )                           = vn
    decrementDimension (Number 0)        vn@( VersionNumber _ _ )                           = vn
    decrementDimension (Number 1)          ( VersionNumber vc vn )                          = VersionNumber ( decrement vc ) vn
    decrementDimension (Number 2)          ( VersionNumber vc1 vc2@(VersionCompound _ ) )   = VersionNumber vc1 ( decrement vc2 )
    decrementDimension dim                 ( VersionNumber vc1 vc2@(VersionCompound _ ) )   = VersionNumber vc1 vc2
    decrementDimension dim                 ( VersionNumber vc vn@(VersionNumber _ _ ) )     = VersionNumber vc ( decrementDimension ( decrement dim ) vn )
    increment                              ( VersionCompound vc )                           = VersionCompound ( increment vc )
    increment                              ( VersionNumber vc vn )                          = VersionNumber vc ( increment vn )
    incrementDimension (NumberPlaceholder) ( VersionCompound vc )                           = VersionCompound vc
    incrementDimension (Number 0)          ( VersionCompound vc )                           = VersionCompound vc
    incrementDimension (Number 1)          ( VersionCompound vc )                           = VersionCompound ( increment vc )
    incrementDimension dim                 ( VersionCompound vc )                           = VersionCompound ( decrementDimension ( increment dim ) vc )
    incrementDimension NumberPlaceholder vn@( VersionNumber _ _ )                           = vn
    incrementDimension (Number 0)        vn@( VersionNumber _ _ )                           = vn
    incrementDimension (Number 1)          ( VersionNumber vc vn )                          = VersionNumber ( increment vc ) vn
    incrementDimension (Number 2)          ( VersionNumber vc1 vc2@(VersionCompound _ ) )   = VersionNumber vc1 ( increment vc2 )
    incrementDimension dim                 ( VersionNumber vc1 vc2@(VersionCompound _ ) )   = VersionNumber vc1 vc2
    incrementDimension dim                 ( VersionNumber vc vn@(VersionNumber _ _ ) )     = VersionNumber vc ( incrementDimension ( decrement dim ) vn )
    getNumberOfDimensions (VersionCompound _) = (Number 1)
    getNumberOfDimensions (VersionNumber vc vn ) = increment (getNumberOfDimensions vn)
    appendDimension vn = VersionNumber NumberPlaceholder vn

createVersionNumberByNumberOfDimensions :: NumberOfDimensions -> VersionNumber
createVersionNumberByNumberOfDimensions ( NumberPlaceholder ) = VersionCompound NumberPlaceholder
createVersionNumberByNumberOfDimensions ( Number 0 ) = VersionCompound NumberPlaceholder
createVersionNumberByNumberOfDimensions ( Number 1 ) = VersionCompound NumberPlaceholder
createVersionNumberByNumberOfDimensions num = VersionNumber NumberPlaceholder ( createVersionNumberByNumberOfDimensions ( decrement num ) )

versionCompoundToString :: VersionCompound -> String
versionCompoundToString (Number n) = (show n)
versionCompoundToString NumberPlaceholder = "x"

maybeToString :: Maybe Int -> String
maybeToString (Just n) = (show n)
maybeToString Nothing = "x"

versionNumberToString :: VersionNumber -> String
versionNumberToString (VersionNumber vc vn) = (versionCompoundToString vc) ++ "." ++ (versionNumberToString vn)
versionNumberToString (VersionCompound vc) = (versionCompoundToString vc)

versionNumberWithMaybeToString :: VersionNumberWithMaybe -> String
versionNumberWithMaybeToString (VN vc vn) = (maybeToString vc) ++ "." ++ (versionNumberWithMaybeToString vn)
versionNumberWithMaybeToString (VC vc) = (maybeToString vc)

-- instance Show VersionCompound where
    -- show (Number n) = (show n)
    -- show NumberPlaceholder = "X" 


initialVersionCompound :: VersionCompound
initialVersionCompound = NumberPlaceholder

isInitialVersionCompound :: VersionCompound -> Bool
isInitialVersionCompound NumberPlaceholder = True
isInitialVersionCompound _ = False

initialVersionNumber :: NumberOfDimensions -> VersionNumber
initialVersionNumber dim = (createVersionNumberByNumberOfDimensions dim)

-- isInitialVersionNumberN :: NumberOfDimensions -> VersionNumber -> Bool
-- isInitialVersionNumberN (NumberPlaceholder) (VersionCompound NumberPlaceholder ) = True
-- isInitialVersionNumberN (Number 0) (VersionCompound NumberPlaceholder ) = True
-- isInitialVersionNumberN (Number 1) (VersionCompound NumberPlaceholder ) = True
-- isInitialVersionNumberN dim (VersionNumber NumberPlaceholder vn) = isInitialVersionNumberN (decrement dim) vn
-- isInitialVersionNumberN _ _ = False

isInitialVersionNumber :: VersionNumber -> Bool
isInitialVersionNumber ( VersionCompound NumberPlaceholder ) = True
isInitialVersionNumber ( VersionCompound ( Number 0 )) = False
isInitialVersionNumber ( VersionCompound ( Number 1 )) = False
isInitialVersionNumber ( VersionCompound ( Number _ )) = False
isInitialVersionNumber ( VersionNumber vc vn ) = ( isInitialVersionCompound vc ) && ( isInitialVersionNumber vn )
isInitialVersionNumber _ = False

incrementReleaseNumber :: VersionNumber -> VersionNumber
incrementReleaseNumber (VersionNumber vc@(Number _) vn@(VersionCompound _)) = VersionNumber (increment vc) vn
incrementReleaseNumber (VersionNumber vc@(NumberPlaceholder) vn@(VersionCompound _)) = VersionNumber (increment vc) vn
incrementReleaseNumber (VersionNumber vc vn) = VersionNumber vc (incrementReleaseNumber vn)
{-incrementReleaseNumber (VersionNumber _ vn) = VersionNumber-}

incrementSupportNumber :: VersionNumber -> VersionNumber
incrementSupportNumber (VersionNumber vc@(Number _) vn@(VersionNumber NumberPlaceholder (VersionCompound _ ))) = VersionNumber (increment vc) vn
incrementSupportNumber (VersionNumber vc@( _ ) vn@(VersionNumber _ (VersionCompound _ ))) = VersionNumber (increment vc) vn
incrementSupportNumber (VersionNumber vc@( _ ) vn@(VersionCompound _ )) = VersionNumber vc vn
incrementSupportNumber (VersionNumber vc vn) = VersionNumber vc (incrementSupportNumber vn)

-- BRANCH DETECTORS

class IsExperimentalBranch a where
        isExperimentalBranch :: a -> Bool

instance IsExperimentalBranch VersionNumber where
        isExperimentalBranch (VersionCompound NumberPlaceholder) = True
        isExperimentalBranch (VersionNumber (NumberPlaceholder) vn) = isExperimentalBranch vn
        isExperimentalBranch _ = False

class IsReleaseBranch a where 
        isReleaseBranch :: a -> Bool

instance IsReleaseBranch VersionNumber where
        isReleaseBranch (VersionNumber vc@(Number _) vn@(VersionCompound NumberPlaceholder)) = True
        isReleaseBranch (VersionNumber vc vn) = isReleaseBranch vn 
        isReleaseBranch _ = False 

class IsSupportBranch a where 
        isSupportBranch :: a -> Bool

instance IsSupportBranch VersionNumber where
        isSupportBranch (VersionNumber (Number _) (VersionNumber NumberPlaceholder (VersionCompound NumberPlaceholder))) = True
        isSupportBranch (VersionNumber vc vn) = isSupportBranch vn 
        isSupportBranch _ = False 

-- SNAPSHOT DETECTORS

class IsExperimentalSnapshot a where 
        isExperimentalSnapshot :: a -> Bool

instance IsExperimentalSnapshot VersionNumber where
        isExperimentalSnapshot (VersionCompound (Number _)) = True
        isExperimentalSnapshot (VersionNumber (NumberPlaceholder) vn) = isExperimentalSnapshot vn
        isExperimentalSnapshot _ = False

class IsReleaseSnapshot a where 
        isReleaseSnapshot :: a -> Bool

instance IsReleaseSnapshot VersionNumber where
        isReleaseSnapshot (VersionNumber (Number _ ) (VersionCompound (Number _))) = True 
        isReleaseSnapshot (VersionNumber (NumberPlaceholder) vn) = isReleaseSnapshot vn
        isReleaseSnapshot _ = False

class IsSupportSnapshot a where
        isSupportSnapshot :: a -> Bool

instance IsSupportSnapshot VersionNumber where
        isSupportSnapshot (VersionNumber (Number _) (VersionNumber NumberPlaceholder (VersionCompound (Number _)))) = True
        isSupportSnapshot (VersionNumber (NumberPlaceholder) vn) = isSupportSnapshot vn
        isSupportSnapshot _ = False

selectLatestVersionNumber :: [VersionNumber] -> VersionNumber
selectLatestVersionNumber [] = initialVersionNumber (NumberPlaceholder)
selectLatestVersionNumber (x:xs) = max x (selectLatestVersionNumber xs)

parseVersionCompound :: Parser VersionCompound
parseVersionCompound =
     ( string "x"    >> return NumberPlaceholder)
 <|> ( string "X"    >> return NumberPlaceholder)
 <|> ( decimal >>= \num -> return (Number num) )
      
stringToVersionCompound :: String -> VersionCompound
stringToVersionCompound str = case (parseOnly parseVersionCompound $ BS.pack str) of
    Right a -> a
    Left _ -> initialVersionCompound

parseVersionNumber :: Parser VersionNumber
parseVersionNumber = do
    ds <- sepBy1 parseVersionCompound (char '.')
    let vs = map VersionCompound ds
    return (foldr1 (\(VersionCompound vc) -> VersionNumber vc) vs )

stringToVersionNumber :: String -> VersionNumber
stringToVersionNumber str = case (parseOnly parseVersionNumber $ BS.pack str) of
    Right a -> a
    Left _ -> createVersionNumberByNumberOfDimensions (Number 0)

-- instance JSON.FromJSON VersionCompound where
    -- parseJSON (JSON.Object v) = (parse parseVersionCompound v)
    -- parseJSON _ = mzero
    
instance Eq VersionCompound where
    NumberPlaceholder == NumberPlaceholder = True
    (Number v1) == (Number v2)            = (v1 == v2)
    _ == _                                 = False
    
instance Ord VersionCompound where
    NumberPlaceholder `compare` NumberPlaceholder = EQ
    n@(Number _ ) `compare` NumberPlaceholder = GT -- error "Cannot compare numbers and number placeholders"
    NumberPlaceholder `compare` n@(Number _ ) = LT -- error "Cannot compare number placeholders and numbers"
    (Number v1) `compare` (Number v2) = (v1 `compare` v2)
    
instance Eq VersionNumber where
    ( VersionCompound vc1 ) == ( VersionCompound vc2 ) = (vc1 == vc2)
    ( VersionNumber vc1 vn1 ) == ( VersionNumber vc2 vn2 ) = (vc1 == vc2 && vn1 == vn2)
    ( VersionNumber vc1 vn1 ) == ( VersionCompound vc2 ) = (vc1 == vc2 && vn1 == (VersionCompound NumberPlaceholder) ) 
    ( VersionCompound vc1 ) == ( VersionNumber vc2 vn2) = (vc1 == vc2 && vn2 == (VersionCompound NumberPlaceholder) )
        
instance Ord VersionNumber where
    (VersionCompound vc1)   `compare` (VersionCompound vc2)     = (vc1 `compare` vc2)
    (VersionNumber vc1 vn1) `compare` (VersionCompound vc2)     = case (VersionCompound vc1 `compare` stringToVersionNumber "x") of
        EQ -> (vn1 `compare` VersionCompound vc2)
        LT -> LT
        GT -> GT
    (VersionCompound vc1)   `compare` (VersionNumber vc2 vn2)   = case (stringToVersionNumber "x" `compare` VersionCompound vc2) of
        EQ -> (VersionCompound vc1 `compare` vn2)
        LT -> LT
        GT -> GT
    (VersionNumber vc1 vn1) `compare` (VersionNumber vc2 vn2)   = case (vc1 `compare` vc2) of
        EQ -> (vn1 `compare` vn2)
        LT -> LT
        GT -> GT

    {-( VersionCompound vc1 ) `compare` ( VersionNumber vc2 vn2 ) = case (isInitialVersionNumber vn2) of -}
        {-False -> (VersionCompound NumberPlaceholder) `compare` vn2-}
        {-True -> (vc1 `compare` vc2) -}
    {-( VersionNumber vc1 vn1) `compare` ( VersionCompound vc2 ) = case (isInitialVersionNumber vn1) of -}
        {-False -> (VersionCompound NumberPlaceholder) `compare` vn1-}
        {-True -> (vc1 `compare` vc2) -}
    {-( VersionNumber vc1 vn1 ) `compare` ( VersionNumber vc2 vn2 ) = case (vc1 `compare` vc2) of -}
            {-EQ -> vn1 `compare` vn2-}
            {-LT -> LT-}
            {-GT -> GT-}

freezeDimension :: VersionCompound -> VersionCompound
freezeDimension NumberPlaceholder = (Number 0)
freezeDimension (Number n) = Number n
class FreezeDimension a where 
        freezeDimensionByNum :: NumberOfDimensions -> a -> a

instance FreezeDimension VersionNumber where
        freezeDimensionByNum NumberPlaceholder   vc@(VersionCompound _ )                = vc
        freezeDimensionByNum NumberPlaceholder   vn@(VersionNumber _ _)                 = vn
        freezeDimensionByNum (Number 0)          vc@(VersionCompound _ )                = vc
        freezeDimensionByNum (Number 0)          vn@(VersionNumber _ _)                 = vn
        freezeDimensionByNum (Number 1)          (VersionCompound vc)                   = (VersionCompound (freezeDimension vc) )
        freezeDimensionByNum (Number 1)          (VersionNumber vc vn)                  = ( VersionNumber ( freezeDimension vc ) vn ) 
        freezeDimensionByNum (Number n)          vc@(VersionCompound NumberPlaceholder) = vc
        freezeDimensionByNum num                 (VersionNumber vc vn)                  = (VersionNumber vc (freezeDimensionByNum (decrement num) vn ))











