{-# LANGUAGE OverloadedStrings #-}

module VersionNumber where

import Data.Attoparsec.Char8
-- import Data.ByteString.Char8
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.Text as T
-- import System.Directory

type NumberOfDimensions = Int

data VersionCompound = NumberPlaceholder                      -- X
                     | Number Int                           -- 1, 2, 3, ..., 45, ... 
                     deriving (Show)

data VersionNumber = VersionCompound VersionCompound
       | VersionNumber VersionCompound VersionNumber
       deriving (Show)

createVersionNumberByDimensionNumber :: NumberOfDimensions -> VersionNumber
createVersionNumberByDimensionNumber 0 = VersionCompound NumberPlaceholder
createVersionNumberByDimensionNumber 1 = VersionCompound NumberPlaceholder
createVersionNumberByDimensionNumber num = VersionNumber NumberPlaceholder ( createVersionNumberByDimensionNumber (num - 1) )

-- instance Monad VersionCompound where
    -- return x = stringToVersionCompound x
    -- x >>= f = f $ x

versionCompoundToString :: VersionCompound -> String
versionCompoundToString (Number n) = (show n)
versionCompoundToString NumberPlaceholder = "X"

versionNumberToString :: VersionNumber -> String
versionNumberToString (VersionNumber vc vn) = (versionCompoundToString vc) ++ "." ++ (versionNumberToString vn)
versionNumberToString (VersionCompound vc) = (versionCompoundToString vc)

-- instance Show VersionCompound where
    -- show (Number n) = (show n)
    -- show NumberPlaceholder = "X" 

stringToVersionCompound :: Parser VersionCompound
stringToVersionCompound =
     ( string "X"    >> return NumberPlaceholder)
 <|> ( decimal >>= \num -> return (Number num) )
      
-- instance JSON.FromJSON VersionCompound where
    -- parseJSON (JSON.Object v) = (parse stringToVersionCompound v)
    -- parseJSON _ = mzero
    
instance Eq VersionCompound where
    NumberPlaceholder == NumberPlaceholder = True
    (Number v1) == (Number v2)            = (v1 == v2)
    _ == _                                 = False
    
instance Ord VersionCompound where
    NumberPlaceholder `compare` NumberPlaceholder = error "Cannot compare number placeholders"
    (Number _ ) `compare` NumberPlaceholder = error "Cannot compare numbers and number placeholders"
    NumberPlaceholder `compare` (Number _ ) = error "Cannot compare number placeholders and numbers"
    (Number v1) `compare` (Number v2) = (v1 `compare` v2)
    
instance Eq VersionNumber where
    ( VersionCompound vc1 ) == ( VersionCompound vc2 ) = (vc1 == vc2)
    ( VersionNumber vc1 vn1 ) == ( VersionNumber vc2 vn2 ) = (vc1 == vc2 && vn1 == vn2)
    _ == _                                 = False
        
instance Ord VersionNumber where
    ( VersionCompound vc1 ) `compare` ( VersionCompound vc2 ) = (vc1 `compare` vc2)
    ( VersionCompound vc1 ) `compare` ( VersionNumber vc2 vn2 ) = error "Cannot compare version numbers of different length"
    ( VersionNumber vc1 vn1) `compare` ( VersionCompound vc2 ) = error "Cannot compare version numbers of different length"
    ( VersionNumber vc1 vn1 ) `compare` ( VersionNumber vc2 vn2 ) = case (vn1 `compare` vn2) of 
            EQ -> vc1 `compare` vc2
            LT -> case (vc1 `compare` vc2) of
                EQ -> LT
                _  -> error "Cannot compare version numbers"
            GT -> case (vc1 `compare` vc2) of
                EQ -> GT
                _  -> error "Cannot compare version numbers"
    