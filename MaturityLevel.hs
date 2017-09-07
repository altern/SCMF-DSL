module MaturityLevel where

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import VersionNumber 

data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Prod
                   deriving (Show, Enum, Ord, Eq, Read)

stringToMaturity :: String -> MaturityLevel
stringToMaturity str = read str

instance VersionOperations MaturityLevel where
    increment ml = succ ml
    decrement ml = pred ml
    freeze ml = ml
    incrementDimension 1 ml = succ ml
    incrementDimension _ ml = ml
    decrementDimension 1 ml = pred ml
    decrementDimension _ ml = ml
    freezeDimension _ ml = ml
