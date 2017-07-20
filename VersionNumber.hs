{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module VersionNumber where

import Data.Attoparsec.Char8
import Control.Applicative
import Control.Monad
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS


generateNewVC :: (Maybe Int) -> (Maybe Int)
generateNewVC Nothing = Nothing
generateNewVC (Just num) = Just (num + 1)

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
        increment Nothing = Nothing
        increment (Just num) = (Just (num + 1))

data VersionNumber = VC (Maybe Int)
		| VN VersionNumber (Maybe Int)
		deriving (Show)

generateNewVersionNumber :: VersionNumber -> VersionNumber
generateNewVersionNumber ( VC vc ) = ( VC (generateNewVC vc) )
generateNewVersionNumber ( VN vn vc ) = ( VN vn (generateNewVC vc) )


createVersionNumberByNumberOfDimensions :: (Maybe Int) -> VersionNumber
createVersionNumberByNumberOfDimensions ( Nothing ) = VC Nothing
createVersionNumberByNumberOfDimensions ( Just 0 ) = VC Nothing
createVersionNumberByNumberOfDimensions ( Just 1 ) = VC Nothing
createVersionNumberByNumberOfDimensions num = VN (createVersionNumberByNumberOfDimensions ( decrement num )) Nothing 

versionCompoundToString :: (Maybe Int)-> String
versionCompoundToString (Just n) = (show n)
versionCompoundToString Nothing = "x"

maybeToString :: Maybe Int -> String
maybeToString (Just n) = (show n)
maybeToString Nothing = "x"

versionNumberToString :: VersionNumber -> String
versionNumberToString (VN vn vc) = (versionNumberToString vn) ++ "." ++ (maybeToString vc)
versionNumberToString (VC vc) = (maybeToString vc)

-- instance Show VC where
    -- show (Just n) = (show n)
    -- show Nothing = "X" 


initialVersionNumber :: (Maybe Int) -> VersionNumber
initialVersionNumber dim = (createVersionNumberByNumberOfDimensions dim)

-- isInitialVersionNumberN :: (Maybe Int) -> VersionNumber -> Bool
-- isInitialVersionNumberN (Nothing) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 0) (VC Nothing ) = True
-- isInitialVersionNumberN (Just 1) (VC Nothing ) = True
-- isInitialVersionNumberN dim (VersionNumber Nothing vn) = isInitialVersionNumberN (decrement dim) vn
-- isInitialVersionNumberN _ _ = False

isInitialVersionNumber :: VersionNumber -> Bool
isInitialVersionNumber ( VC Nothing ) = True
isInitialVersionNumber ( VC ( Just 0 )) = False
isInitialVersionNumber ( VC ( Just 1 )) = False
isInitialVersionNumber ( VC ( Just _ )) = False
isInitialVersionNumber ( VN vn vc@(Just _) ) = False
isInitialVersionNumber ( VN vn vc@(Nothing) ) = ( isInitialVersionNumber vn )
isInitialVersionNumber _ = False

{-selectLatestVersionNumber :: [VersionNumber] -> VersionNumber-}
{-selectLatestVersionNumber [] = initialVersionNumber (Nothing)-}
{-selectLatestVersionNumber (x:xs) = max x (selectLatestVersionNumber xs)-}

parseVC :: Parser (Maybe Int)
parseVC =
     ( string "x"    >> return Nothing)
 <|> ( string "X"    >> return Nothing)
 <|> ( decimal >>= \num -> return (Just num) )
      
stringToVCWithMaybe :: String -> (Maybe Int) 
stringToVCWithMaybe str = case (parseOnly parseVC $ BS.pack str) of
    Right a -> a
    Left _ -> Nothing

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

stringToVC :: String -> (Maybe Int)
stringToVC str = case (parseOnly parseVC $ BS.pack str) of
    Right a -> a
    Left _ -> Nothing

freezeDimension :: (Maybe Int) -> (Maybe Int)
freezeDimension Nothing = (Just 0)
freezeDimension (Just n) = Just n
class FreezeDimension a where 
        freezeDimensionByNum :: (Maybe Int) -> a -> a
