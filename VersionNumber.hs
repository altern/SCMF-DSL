{-# LANGUAGE OverloadedStrings #-}

module VersionNumber where

import Data.Attoparsec.Char8
-- import Data.ByteString.Char8
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.Text as T
-- import System.Directory


data VersionNumber = NumberPlaceholder                      -- X
                   | Number Int                           -- 1, 2, 3, ..., 45, ... 
                     deriving (Show)

versionNumberToString :: VersionNumber -> String
versionNumberToString (Number n) = (show n)
versionNumberToString NumberPlaceholder = "X"

-- instance Show VersionNumber where
    -- show (Number n) = (show n)
    -- show NumberPlaceholder = "X" 

stringToVersionNumber :: Parser VersionNumber
stringToVersionNumber =
     ( string "X"    >> return NumberPlaceholder)
 <|> ( decimal >>= \num -> return (Number num) )
      
-- instance JSON.FromJSON VersionNumber where
    -- parseJSON (JSON.Object v) = (parse stringToVersionNumber v)
    -- parseJSON _ = mzero
    
instance Eq VersionNumber where
    NumberPlaceholder == NumberPlaceholder = True
    (Number v1) == (Number v2)            = (v1 == v2)
    _ == _                                 = False
    
instance Ord VersionNumber where
    NumberPlaceholder `compare` NumberPlaceholder = error "Cannot compare number placeholders"
    (Number _ ) `compare` NumberPlaceholder = error "Cannot compare numbers and number placeholders"
    NumberPlaceholder `compare` (Number _ ) = error "Cannot compare number placeholders and numbers"
    (Number v1) `compare` (Number v2) = (v1 `compare` v2)