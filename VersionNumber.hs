{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module VersionNumber where

import Data.Attoparsec.Char8
import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

class VersionOperations a where
    decrement :: a -> a
    decrementDimension :: (Maybe Int) -> a -> a
    increment :: a -> a
    incrementDimension :: (Maybe Int) -> a -> a
    getNumberOfDimensions :: a -> (Maybe Int)
    appendDimension :: a -> a

instance VersionOperations (Maybe Int) where
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

data VersionNumber = VC (Maybe Int)
        | VN VersionNumber (Maybe Int)
        deriving (Show)

instance VersionOperations VersionNumber where
        decrement (VC vc) = VC (decrement vc)
        decrement (VN vn vc) = VN vn (decrement vc)
        decrementDimension dim (VC vc) = VC (decrementDimension dim vc)
        decrementDimension dim@(Just 1) (VN vn vc) = VN vn (decrementDimension dim vc)
        decrementDimension dim (VN vn vc) = VN (decrementDimension (decrement dim) vn) vc
        increment (VC vc) = VC (increment vc)
        increment (VN vn vc) = VN vn (increment vc)
        incrementDimension dim (VC vc) = VC (incrementDimension dim vc)
        incrementDimension dim@(Just 1) (VN vn vc) = VN vn (incrementDimension dim vc)
        incrementDimension dim (VN vn vc) = VN (incrementDimension (decrement dim) vn) vc
        getNumberOfDimensions (VC vc) = (getNumberOfDimensions vc)
        getNumberOfDimensions (VN vn vc) = increment (getNumberOfDimensions vn)
        appendDimension (VC vc) = VN (VC Nothing) vc
        appendDimension (VN vn vc) = VN (appendDimension vn) vc

instance Eq VersionNumber where
        (VC vc1) == (VC vc2) = (vc1 == vc2)
        ( VN vn1 vc1 ) == ( VN vn2 vc2 ) = (vc1 == vc2 && vn1 == vn2)
        ( VN _ vc1 ) == (VC vc2) = vc1 == vc2
        ( VC vc1 ) == (VN _ vc2) = vc1 == vc2

instance Ord VersionNumber where
    (VC vc1)   `compare` (VC vc2)     = (vc1 `compare` vc2)
    (VN vn1 vc1) `compare` (VC vc2)     = case (vn1 `compare` VC Nothing) of
        EQ -> (vc1 `compare` vc2)
        LT -> LT
        GT -> GT
    (VC vc1)   `compare` (VN vn2 vc2)   = case (VC Nothing `compare` vn2) of
        EQ -> (vc1 `compare` vc2)
        LT -> LT
        GT -> GT
    (VN vn1 vc1) `compare` (VN vn2 vc2)   = case (vn1 `compare` vn2) of
        EQ -> (vc1 `compare` vc2)
        LT -> LT
        GT -> GT

createVersionNumberByNumberOfDimensions :: (Maybe Int) -> VersionNumber
createVersionNumberByNumberOfDimensions ( Nothing ) = VC Nothing
createVersionNumberByNumberOfDimensions ( Just 0 ) = VC Nothing
createVersionNumberByNumberOfDimensions ( Just 1 ) = VC Nothing
createVersionNumberByNumberOfDimensions num = VN (createVersionNumberByNumberOfDimensions ( decrement num )) Nothing 

versionCompoundToString :: (Maybe Int)-> String
versionCompoundToString (Just n) = (show n)
versionCompoundToString Nothing = "x"

versionNumberToString :: VersionNumber -> String
versionNumberToString (VN vn vc) = (versionNumberToString vn) ++ "." ++ (versionCompoundToString vc)
versionNumberToString (VC vc) = (versionCompoundToString vc)

-- instance Show VC where
    -- show (Just n) = (show n)
    -- show Nothing = "X" 

-- isInitialVersionNumberN :: (Maybe Int) -> VersionNumber -> Bool
-- isInitialVersionNumberN (Nothing) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 0) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 1) (VC Nothing ) = True
-- isInitialVersionNumberN dim (VersionNumber Nothing vn) = isInitialVersionNumberN (decrement dim) vn
-- isInitialVersionNumberN _ _ = False

isInitialVersionNumber :: VersionNumber -> Bool
isInitialVersionNumber ( VC Nothing ) = True
isInitialVersionNumber ( VC ( Just _ )) = False
isInitialVersionNumber ( VN vn vc@(Just _) ) = False
isInitialVersionNumber ( VN vn vc@(Nothing) ) = ( isInitialVersionNumber vn )

isReleaseVersionNumber :: VersionNumber -> Bool
isReleaseVersionNumber (VC vc) = False
isReleaseVersionNumber (VN vn Nothing) = isSnapshotVersionNumber vn
isReleaseVersionNumber _ = False

isSnapshotVersionNumber :: VersionNumber -> Bool
isSnapshotVersionNumber (VC (Just _)) = True
isSnapshotVersionNumber (VN vn Nothing) = False
isSnapshotVersionNumber (VN vn _) = isSnapshotVersionNumber vn 
isSnapshotVersionNumber _ = False

isSupportVersionNumber :: VersionNumber -> Bool
isSupportVersionNumber (VN (VN (VC (Just _)) Nothing) Nothing) = True
isSupportVersionNumber (VN vn Nothing ) = isReleaseVersionNumber vn
isSupportVersionNumber (VN vn (Just _)) = False
isSupportVersionNumber _ = False

{-selectLatestVersionNumber :: [VersionNumber] -> VersionNumber-}
{-selectLatestVersionNumber [] = initialVersionNumber (Nothing)-}
{-selectLatestVersionNumber (x:xs) = max x (selectLatestVersionNumber xs)-}

parseVC :: Parser (Maybe Int)
parseVC =
     ( string "x"    >> return Nothing)
 <|> ( string "X"    >> return Nothing)
 <|> ( decimal >>= \num -> return (Just num) )
      
{-parseVersionNumberL :: Parser VersionNumber-}
{-parseVersionNumberL = do-}
    {-ds <- sepBy1 parseVCWithMaybe (char '.')-}
    {-let vs = map VC ds-}
    {-return (foldr1 (\(VC vc) -> VNL vc) vs )-}

parseVersionNumberR :: Parser VersionNumber
parseVersionNumberR = do
    ds <- sepBy1 parseVC (char '.')
    let vs = map VC ds
    return (foldr1 (\(VC vc) vs -> VN vs vc) (reverse vs))

stringToVersionNumber :: String -> VersionNumber
stringToVersionNumber str = case (parseOnly parseVersionNumberR $ BS.pack str) of
    Right a -> a
    Left _ -> VC Nothing


