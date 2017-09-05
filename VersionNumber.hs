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
    freeze :: a -> a
    incrementDimension :: Int -> a -> a
    freezeDimension :: Int -> a -> a
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
    freeze Nothing = (Just 0)
    freeze (Just n) = (Just n)
    incrementDimension 1 vc = increment vc
    incrementDimension _ vc = vc
    freezeDimension 1 vc = freeze vc
    freezeDimension _ vc = vc
    getNumberOfDimensions _ = 1
    appendDimension vc = vc

instance VersionOperations VersionNumber where
    decrement (VersionNumber vn) = VersionNumber ( replaceNth (length vn - 1) (decrement $ last vn) vn)
    decrementDimension dim (VersionNumber vn) = (VersionNumber ( replaceNth (length vn - dim) (decrement $ vn!!(length vn - dim)) vn))
    increment (VersionNumber vn) = VersionNumber ( replaceNth (length vn - 1) (increment $ last vn) vn)
    freeze (VersionNumber vn) = VersionNumber ( replaceNth (length vn - 1) (freeze $ last vn) vn)
    incrementDimension dim (VersionNumber vn) = (VersionNumber ( replaceNth (length vn - dim) (increment $ vn!!(length vn - dim)) vn))
    freezeDimension dim (VersionNumber vn) = (VersionNumber ( replaceNth (length vn - dim) (freeze $ vn!!(length vn - dim)) vn))
    getNumberOfDimensions (VersionNumber vn) = length vn
    appendDimension (VersionNumber vn) = VersionNumber $ [Nothing] ++ vn
    
instance Eq VersionNumber where
    (VersionNumber a) == (VersionNumber b) = on (==) (dropWhile (Nothing ==)) a b

instance Ord VersionNumber where
    (VersionNumber a) `compare` (VersionNumber b) = on compare (dropWhile (Nothing ==)) a b

createVersionNumberByNumberOfDimensions :: NumberOfDimensions -> VersionNumber
createVersionNumberByNumberOfDimensions num = VersionNumber $ replicate num Nothing

class ToString a where
    toString :: a -> String

instance ToString VersionCompound where
    toString (Just n) = (show n)
    toString Nothing = "x"

instance ToString [VersionCompound] where
    toString [] = "" 
    toString (x:[]) = (toString x)
    toString (x:xs) = (toString x) ++ "." ++ (toString xs) 

instance ToString VersionNumber where
    toString (VersionNumber []) = "" 
    toString (VersionNumber (x:[])) = (toString x) 
    toString (VersionNumber (x:xs)) = (toString x) ++ "." ++ (toString xs)

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
        | dim < length vn = let remainder = dropWhile (Nothing ==) vn 
          in replicate (dim - length remainder) Nothing ++ remainder 
        | dim == length vn = vn
        | dim > length vn = replicate (dim - length vn) Nothing ++ vn

instance MakeDimensional VersionNumber where
      makeNDimensional dim (VersionNumber vn) 
        | dim < length vn = let remainder = dropWhile (Nothing ==) vn 
          in VersionNumber $ replicate (dim - length remainder) Nothing ++ remainder 
        | dim == length vn = VersionNumber vn
        | dim > length vn = VersionNumber $ replicate (dim - length vn) Nothing ++ vn

class VersionDetection a where
        isInitial :: a -> Bool 
        isExperimentalBranch :: a -> Bool
        isReleaseBranch :: a -> Bool
        isSupportBranch :: a -> Bool
        isExperimentalSnapshot :: a -> Bool
        isReleaseSnapshot :: a -> Bool
        isSupportSnapshot :: a -> Bool
        isRevision :: a -> Bool 

applyListOfBoolFunctions :: [VersionCompound -> Bool] -> [VersionCompound] -> Bool
applyListOfBoolFunctions listOfBoolFunctions xs = all (==True) $ zipWith ($) listOfBoolFunctions $ makeNDimensional (length listOfBoolFunctions) $ lastN (length listOfBoolFunctions) $ dropWhile (==Nothing) xs

instance VersionDetection VersionNumber where
        isInitial (VersionNumber vn) = all (==Nothing) vn
        isExperimentalBranch vn = isInitial vn 
        isReleaseBranch (VersionNumber vn) = applyListOfBoolFunctions [isJust, isJust, isNothing] vn || applyListOfBoolFunctions [isJust, isNothing] vn
        isSupportBranch (VersionNumber vn) = applyListOfBoolFunctions [isJust, isNothing, isNothing] vn
        isExperimentalSnapshot (VersionNumber vn) = applyListOfBoolFunctions [isNothing, isNothing, isJust] vn 
        isReleaseSnapshot (VersionNumber vn) = applyListOfBoolFunctions [isJust, isJust, isJust] vn || applyListOfBoolFunctions [isJust, isJust] vn
        isSupportSnapshot (VersionNumber vn) = applyListOfBoolFunctions [isJust, isNothing, isJust] vn
        isRevision (VersionNumber vn) = applyListOfBoolFunctions [isNothing, isNothing, isJust] vn
        
class GenerateNew a where
        generateNewReleaseBranch :: a -> a
        generateNewSupportBranch :: a -> a
        generateNewReleaseSnapshot :: a -> a
        generateNewSupportSnapshot :: a -> a
        generateNewRevision :: a -> a
        generateNewExperimentalSnapshot :: a -> a
        generateNewVersion :: a -> a

instance GenerateNew VersionNumber where
        generateNewReleaseBranch vn = if (isReleaseBranch vn) 
                                      then incrementDimension 2 vn 
                                      else if (isSupportBranch vn)
                                          then freezeDimension 2 vn
                                          else if (isInitial vn)
                                              then freezeDimension 2 $ makeNDimensional 2 vn
                                              else vn
        generateNewSupportBranch vn = if (isSupportBranch vn)
                                      then incrementDimension 3 vn
                                      else if (isInitial vn)
                                          then freezeDimension 3 $ makeNDimensional 3 vn 
                                          else vn
        generateNewReleaseSnapshot vn = if (isReleaseSnapshot vn)
                                        then incrementDimension 1 vn
                                        else if (isReleaseBranch vn)
                                            then freezeDimension 1 $ makeNDimensional 1 vn
                                            else vn
        generateNewSupportSnapshot vn = if (isSupportSnapshot vn)
                                        then incrementDimension 1 vn
                                        else if (isSupportBranch vn)
                                            then freezeDimension 1 $ makeNDimensional 1 vn
                                            else vn
        generateNewExperimentalSnapshot vn = if (isExperimentalSnapshot vn)
                                             then incrementDimension 1 vn
                                             else if (isInitial vn)
                                                then freezeDimension 1 $ makeNDimensional 1 vn
                                                else vn
        generateNewRevision = generateNewExperimentalSnapshot
        
        generateNewVersion vn
            | isReleaseBranch vn = generateNewReleaseBranch vn
            | isSupportBranch vn = generateNewSupportBranch vn
            | isReleaseSnapshot vn = generateNewReleaseSnapshot vn
            | isSupportSnapshot vn = generateNewSupportSnapshot vn
            | isExperimentalSnapshot vn = generateNewExperimentalSnapshot vn
            | isRevision vn = generateNewRevision vn
            | otherwise = vn

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

initialVersionNumber :: VersionNumber
initialVersionNumber = VersionNumber [Nothing]
