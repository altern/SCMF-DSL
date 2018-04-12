{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module MaturityLevel where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import VersionNumber 

data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Prod
                   deriving (Show, Enum, Ord, Eq, Read, JSON.ToJSON, JSON.FromJSON, Generic)

initialMaturity :: MaturityLevel
initialMaturity = Dev

parseMaturity :: Parser MaturityLevel
parseMaturity = 
        ( string (BS.pack "Dev") >> return Dev)
    <|> ( string (BS.pack "Test") >> return Test)
    <|> ( string (BS.pack "User") >> return User)
    <|> ( string (BS.pack "ReleaseCandidate") >> return ReleaseCandidate)
    <|> ( string (BS.pack "Prod") >> return Prod)
    
{-stringToMaturity :: String -> MaturityLevel-}
{-stringToMaturity str = case (parseOnly parseMaturity $ BS.pack str) of-}
    {-Right a -> a-}
    {-Left _ -> initialMaturity-}

instance VersionOperations MaturityLevel where
    increment Prod = Prod
    increment ml = succ ml
    decrement Dev = Dev
    decrement ml = pred ml
    freeze ml = ml
    incrementDimension 1 ml = succ ml
    incrementDimension _ ml = ml
    decrementDimension 1 ml = pred ml
    decrementDimension _ ml = ml
    freezeDimension _ ml = ml
    
incrementSupport :: MaturityLevel -> MaturityLevel
incrementSupport User = User
incrementSupport ml = increment ml

