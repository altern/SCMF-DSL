{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module VersionNumber where

import Util
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Function(on)
import Data.Maybe

type VersionCompound = Maybe Int

type NumberOfDimensions = Int

data VersionNumber = VersionNumber [VersionCompound]
    deriving (Show)

class VersionOperations a where
    decrement :: a -> a
    decrementDimension :: Int -> a -> a
    increment :: a -> a
    incrementDimension :: Int -> a -> a
    getNumberOfDimensions :: a -> Int
    appendDimension :: a -> a

instance VersionOperations VersionCompound where
        decrement Nothing = Nothing
        decrement (Just 0) = Just 0
        decrement (Just num) = Just (num - 1)
        decrementDimension 1 vc = decrement vc
        decrementDimension _ vc = vc
        increment Nothing = Nothing
        increment (Just num) = (Just (num + 1))
        incrementDimension 1 vc = increment vc
        incrementDimension _ vc = vc
        getNumberOfDimensions _ = 1
        appendDimension vc = vc

instance VersionOperations VersionNumber where
        decrement (VersionNumber vn) = VersionNumber ( replaceNth (length vn - 1) (decrement $ last vn) vn)
        decrementDimension dim (VersionNumber vn) = (VersionNumber ( replaceNth (length vn - dim) (decrement $ vn!!(length vn - dim)) vn))
        increment (VersionNumber vn) = VersionNumber ( replaceNth (length vn - 1) (increment $ last vn) vn)
        incrementDimension dim (VersionNumber vn) = (VersionNumber ( replaceNth (length vn - dim) (increment $ vn!!(length vn - dim)) vn))
        getNumberOfDimensions (VersionNumber vn) = length vn
        appendDimension (VersionNumber vn) = VersionNumber $ [Nothing] ++ vn
        
instance Eq VersionNumber where
    (VersionNumber a) == (VersionNumber b) = on (==) (dropWhile (Nothing ==)) a b

instance Ord VersionNumber where
    (VersionNumber a) `compare` (VersionNumber b) = on compare (dropWhile (Nothing ==)) a b

createVersionNumberByNumberOfDimensions :: NumberOfDimensions -> VersionNumber
createVersionNumberByNumberOfDimensions num = VersionNumber $ replicate num Nothing

versionCompoundToString :: VersionCompound-> String
versionCompoundToString (Just n) = (show n)
versionCompoundToString Nothing = "x"

versionCompoundListToString :: [VersionCompound] -> String
versionCompoundListToString [] = "" 
versionCompoundListToString (x:[]) = (versionCompoundToString x)
versionCompoundListToString (x:xs) = (versionCompoundToString x) ++ "." ++ (versionCompoundListToString xs) 


versionNumberToString :: VersionNumber -> String
versionNumberToString (VersionNumber []) = "" 
versionNumberToString (VersionNumber (x:[])) = (versionCompoundToString x) 
versionNumberToString (VersionNumber (x:xs)) = (versionCompoundToString x) ++ "." ++ (versionCompoundListToString xs)

-- instance Show VC where
    -- show (Just n) = (show n)
    -- show Nothing = "X" 

-- isInitialVersionNumberN :: (Maybe Int) -> VersionNumber -> Bool
-- isInitialVersionNumberN (Nothing) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 0) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 1) (VC Nothing ) = True
-- isInitialVersionNumberN dim (VersionNumber Nothing vn) = isInitialVersionNumberN (decrement dim) vn
-- isInitialVersionNumberN _ _ = False

class MakeDimensional a where
      makeNDimensional :: NumberOfDimensions -> a -> a

instance MakeDimensional [VersionCompound] where
      makeNDimensional dim vn 
        | dim <= length vn = vn
        | dim > length vn = replicate (dim - length vn) Nothing ++ vn

instance MakeDimensional VersionNumber where
      makeNDimensional dim (VersionNumber vn) 
        | dim <= length vn = VersionNumber vn
        | dim > length vn = VersionNumber $ replicate (dim - length vn) Nothing ++ vn

class VersionDetection a where
        isInitial :: a -> Bool 
        isExperimentalBranch :: a -> Bool
        isReleaseBranch :: a -> Bool
        isSupportBranch :: a -> Bool
        isReleaseSnapshot :: a -> Bool
        isSupportSnapshot :: a -> Bool

applyListOfBoolFunctions :: [VersionCompound -> Bool] -> [VersionCompound] -> Bool
applyListOfBoolFunctions listOfBoolFunctions xs = all (==True) $ zipWith ($) listOfBoolFunctions $ makeNDimensional (length listOfBoolFunctions) $ lastN (length listOfBoolFunctions) $ dropWhile (==Nothing) xs

instance VersionDetection VersionNumber where
        isInitial (VersionNumber vn) = all (==Nothing) vn
        isExperimentalBranch vn = isInitial vn 
        isReleaseBranch (VersionNumber vn) = applyListOfBoolFunctions [isJust, isNothing] vn
        isSupportBranch (VersionNumber vn) = applyListOfBoolFunctions [isJust, isNothing, isNothing] vn
        isReleaseSnapshot (VersionNumber vn) = applyListOfBoolFunctions [isJust, isJust, isJust] vn
        isSupportSnapshot (VersionNumber vn) = applyListOfBoolFunctions [isJust, isNothing, isJust] vn
        
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
    return (VersionNumber ds)

stringToVersionNumber :: String -> VersionNumber
stringToVersionNumber str = case (parseOnly parseVersionNumber $ BS.pack str) of
    Right a -> a
    Left _ -> VersionNumber []

