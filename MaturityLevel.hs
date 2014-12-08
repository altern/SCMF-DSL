module MaturityLevel where

import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

data MaturityLevel = Dev
                   | Test
                   | User
                   | ReleaseCandidate
                   | Prod
                   deriving (Show, Enum, Ord, Eq)

	
parseMaturity :: Parser MaturityLevel
parseMaturity = 
		( string (BS.pack "Dev") >> return Dev)
	<|> ( string (BS.pack "Test") >> return Test)
	<|> ( string (BS.pack "User") >> return User)
	<|> ( string (BS.pack "ReleaseCandidate") >> return ReleaseCandidate)
	<|> ( string (BS.pack "Prod") >> return Prod)
	
stringToMaturity :: String -> MaturityLevel
stringToMaturity str = case (parseOnly parseMaturity $ BS.pack str) of
	Right a -> a
	Left _ -> Dev
	
incrementMaturityLevel :: MaturityLevel -> MaturityLevel
incrementMaturityLevel Prod = Prod
incrementMaturityLevel ml = succ ml

decrementMaturityLevel :: MaturityLevel -> MaturityLevel
decrementMaturityLevel Dev = Dev
decrementMaturityLevel ml = pred ml