{-# LANGUAGE OverloadedStrings #-}

module MyObj where 

import Data.Attoparsec.Char8
import Data.ByteString.Char8
import Control.Monad
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.Text.Lazy  as L 

data MyObj = Placeholder    
           | Number Int

stringToMyObj :: Parser MyObj
stringToMyObj =
     ( string "X"    >> return Placeholder)
 <|> ( decimal >>= \num -> return (Number num) )

-- instance JSON.FromJSON MyObj where
    -- parseJSON (JSON.String v) = parseOnly $ stringToMyObj 
    -- parseJSON _ = mzero