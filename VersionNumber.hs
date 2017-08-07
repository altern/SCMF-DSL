{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module VersionNumber where

import Data.Attoparsec.Char8
import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

type VersionCompound = Maybe Int

type NumberOfDimensions = Maybe Int

type VersionNumber = [VersionCompound]

class VersionOperations a where
    decrement :: a -> a
    decrementDimension :: VersionCompound -> a -> a
    increment :: a -> a
    incrementDimension :: VersionCompound -> a -> a
    getNumberOfDimensions :: a -> VersionCompound
    appendDimension :: a -> a

instance VersionOperations VersionCompound where
        decrement Nothing = Nothing
        decrement (Just 0) = Just 0
        decrement (Just num) = Just (num - 1)
        decrementDimension (Just 1) vc = decrement vc
        decrementDimension _ vc = vc
        increment Nothing = Nothing
        increment (Just num) = (Just (num + 1))
        incrementDimension (Just 1) vc = increment vc
        incrementDimension _ vc = vc
        getNumberOfDimensions _ = Just 1
        appendDimension vc = vc

{-instance VersionOperations VersionNumber where-}
        {-decrement (VC vc) = VC (decrement vc)-}
        {-decrement (VN vn vc) = VN vn (decrement vc)-}
        {-decrementDimension dim (VC vc) = VC (decrementDimension dim vc)-}
        {-decrementDimension dim@(Just 1) (VN vn vc) = VN vn (decrementDimension dim vc)-}
        {-decrementDimension dim (VN vn vc) = VN (decrementDimension (decrement dim) vn) vc-}
        {-increment (VC vc) = VC (increment vc)-}
        {-increment (VN vn vc) = VN vn (increment vc)-}
        {-incrementDimension dim (VC vc) = VC (incrementDimension dim vc)-}
        {-incrementDimension dim@(Just 1) (VN vn vc) = VN vn (incrementDimension dim vc)-}
        {-incrementDimension dim (VN vn vc) = VN (incrementDimension (decrement dim) vn) vc-}
        {-getNumberOfDimensions (VC vc) = (getNumberOfDimensions vc)-}
        {-getNumberOfDimensions (VN vn vc) = increment (getNumberOfDimensions vn)-}
        {-appendDimension (VC vc) = VN (VC Nothing) vc-}
        {-appendDimension (VN vn vc) = VN (appendDimension vn) vc-}

{-instance Eq VersionNumber where-}
        {-(VC vc1) == (VC vc2) = (vc1 == vc2)-}
        {-( VN vn1 vc1 ) == ( VN vn2 vc2 ) = (vc1 == vc2 && vn1 == vn2)-}
        {-( VN _ vc1 ) == (VC vc2) = vc1 == vc2-}
        {-( VC vc1 ) == (VN _ vc2) = vc1 == vc2-}

{-instance Ord VersionNumber where-}
    {-(VC vc1)   `compare` (VC vc2)     = (vc1 `compare` vc2)-}
    {-(VN vn1 vc1) `compare` (VC vc2)     = case (vn1 `compare` VC Nothing) of-}
        {-EQ -> (vc1 `compare` vc2)-}
        {-LT -> LT-}
        {-GT -> GT-}
    {-(VC vc1)   `compare` (VN vn2 vc2)   = case (VC Nothing `compare` vn2) of-}
        {-EQ -> (vc1 `compare` vc2)-}
        {-LT -> LT-}
        {-GT -> GT-}
    {-(VN vn1 vc1) `compare` (VN vn2 vc2)   = case (vn1 `compare` vn2) of-}
        {-EQ -> (vc1 `compare` vc2)-}
        {-LT -> LT-}
        {-GT -> GT-}

createVersionNumberByNumberOfDimensions :: NumberOfDimensions -> VersionNumber
createVersionNumberByNumberOfDimensions ( Nothing ) = []
createVersionNumberByNumberOfDimensions ( Just 0 ) = []
createVersionNumberByNumberOfDimensions ( Just 1 ) = [Nothing]
createVersionNumberByNumberOfDimensions num = (createVersionNumberByNumberOfDimensions ( decrement num )) ++ [Nothing]

versionCompoundToString :: VersionCompound-> String
versionCompoundToString (Just n) = (show n)
versionCompoundToString Nothing = "x"

versionNumberToString :: VersionNumber -> String
versionNumberToString [] = []
versionNumberToString (x:[]) = (versionCompoundToString x) 
versionNumberToString (x:xs) = (versionCompoundToString x) ++ "." ++ (versionNumberToString xs)

-- instance Show VC where
    -- show (Just n) = (show n)
    -- show Nothing = "X" 

-- isInitialVersionNumberN :: (Maybe Int) -> VersionNumber -> Bool
-- isInitialVersionNumberN (Nothing) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 0) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 1) (VC Nothing ) = True
-- isInitialVersionNumberN dim (VersionNumber Nothing vn) = isInitialVersionNumberN (decrement dim) vn
-- isInitialVersionNumberN _ _ = False
class VersionDetection a where
        isInitial :: a -> Bool 
        isExperimentalBranch :: a -> Bool
        isReleaseBranch :: a -> Bool
        isSupportBranch :: a -> Bool
        isReleaseSnapshot :: a -> Bool
        isSupportSnapshot :: a -> Bool

{-instance VersionDetection VersionNumber where-}
        {-isInitial ( VC Nothing ) = True-}
        {-isInitial ( VC ( Just _ )) = False-}
        {-isInitial ( VN vn vc@(Just _) ) = False-}
        {-isInitial ( VN vn vc@(Nothing) ) = ( isInitial vn )-}
        {-isExperimentalBranch vn = isInitial vn -}
        {-isReleaseBranch (VN (VN (VC (Just _)) (Just _)) Nothing) = True-}
        {-isReleaseBranch (VN (VC (Just _)) Nothing) = True-}
        {-isReleaseBranch _ = False-}
        {-isSupportBranch (VN (VN (VC (Just _)) Nothing) Nothing) = True-}
        {-isSupportBranch _ = False-}
        {-isReleaseSnapshot (VN (VN (VC (Just _)) (Just _)) (Just _)) = True-}
        {-isReleaseSnapshot (VN (VC (Just _)) (Just _)) = True-}
        {-isReleaseSnapshot _ = False-}
        {-isSupportSnapshot (VN (VN (VC (Just _)) Nothing) (Just _)) = True-}
        {-isSupportSnapshot _ = False-}

{-selectLatestVersionNumber :: [VersionNumber] -> VersionNumber-}
{-selectLatestVersionNumber [] = initialVersionNumber (Nothing)-}
{-selectLatestVersionNumber (x:xs) = max x (selectLatestVersionNumber xs)-}

parseVC :: Parser VersionCompound
parseVC =
     ( string "x"    >> return Nothing)
 <|> ( string "X"    >> return Nothing)
 <|> ( decimal >>= \num -> return (Just num) )
      
{-parseVersionNumberL :: Parser VersionNumber-}
{-parseVersionNumberL = do-}
    {-ds <- sepBy1 parseVCWithMaybe (char '.')-}
    {-let vs = map VC ds-}
    {-return (foldr1 (\(VC vc) -> VNL vc) vs )-}

parseVersionNumber :: Parser VersionNumber
parseVersionNumber = do
    ds <- sepBy1 parseVC (char '.')
    return ds

stringToVersionNumber :: String -> VersionNumber
stringToVersionNumber str = case (parseOnly parseVersionNumber $ BS.pack str) of
    Right a -> a
    Left _ -> []

