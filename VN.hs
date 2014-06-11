{-# LANGUAGE OverloadedStrings, DataKinds, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module VN where 

--import qualified Text.ParserCombinators.Parsec as P
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.List 

type ListOfInt = [Int]
data LL a = LL [a] a
    deriving (Show)

generateList :: Int -> ListOfInt
generateList 0 = []
generateList 1 = [0]
generateList n = [0] ++ ( generateList ( n - 1 ) )

-- class LLa a b where 
    -- generateList :: b -> a b
--    concat :: LL a -> LL a -> LL a

-- instance LLa LL Int where 
    -- generateList 0 = LL [] 0 
    -- generateList 1 = LL [0] 1
    -- generateList n = case ( generateList ( n - 1 ) ) of 
        -- LL a b -> LL ( [0] ++ a ) ( b + 1 )

inc :: Int -> ListOfInt ->  ListOfInt
inc 0 (x:[]) = error "index is bigger than number of list elements"
inc 1 (x:xs) = ([(x+1)] ++ xs)
inc num (x:xs) = ( [x] ++ (inc (num - 1) xs ) )

-- instance Monad LL where
    -- return x = generateList x
    -- x >>= f = f x 

data VC = Placeholder
        | Number Int
        deriving (Show)

data V = VersionCompound VC
       | Version VC V
       deriving (Show)
       
data M = Dev
        | User
        | Test
        | RC
        | Prod
        deriving (Show)
        
data MV = MaturityVersion M V
        deriving (Show)

data VPosition = First 
               | Second
               | Third
               deriving (Show)

data VPatternCompound = PatternNumberPlaceholder
                     | PatternAnyNumber 
                     | PatternNumber Int
                     | NumberOrPlaceholder VC
                     | PatternAny
                     deriving (Show)

data VP = VPattern VPatternCompound VP
        deriving (Show)
        
class VNum a where
    generateNext :: a -> V
    
instance VNum VC where
    generateNext Placeholder = ( VersionCompound ( Number 0 ) )
    generateNext ( Number num ) = ( VersionCompound ( Number ( num + 1 ) ) )

instance VNum V where
    generateNext ( Version vc version ) = case ( generateNext vc ) of 
        ( VersionCompound vc1 ) -> ( Version vc1 version ) 
    generateNext ( VersionCompound vc ) = ( VersionCompound vc )
    
vcToString :: VC -> String
vcToString ( Number n ) = ( show n )
vcToString Placeholder = "x"

vnToString :: V -> String
vnToString (Version vc vn) = (vcToString vc) ++ "." ++ (vnToString vn)
vnToString (VersionCompound vc) = (vcToString vc)

mvToString :: MV -> String
mvToString (MaturityVersion m v) = ( show m ) ++ "/" ++ ( vnToString v )

stringToVC :: Parser VC
stringToVC =
     ( string "x"    >> return Placeholder)
 <|> ( decimal >>= \num -> return (Number num) )

stringToMaturity :: Parser M
stringToMaturity =
     ( string "Dev" >> return Dev)
 <|> ( string "Test" >> return Test)
 <|> ( string "User" >> return User)
 <|> ( string "RC" >> return RC)
 <|> ( string "Prod" >> return Prod)
    

-- stringToVN :: Parser V
-- stringToVN = 
     -- ( char '.' >> stringToVC )
-- <|> (return [])

parseV :: Parser V
parseV = do
--  d1 <- stringToVC
--  char '.'
--  d2 <- stringToVC
--  char '.'
--  d3 <- stringToVC
--  return $ Version (d1) (Version (d2) (VersionCompound (d3) ) )
    ds <- sepBy1 stringToVC (char '.')
    let vs = map VersionCompound ds
    return (foldr1 (\(VersionCompound vc) -> Version vc) vs )

parseMV :: Parser MV
parseMV = do
    maturity <- stringToMaturity
    char '/'
    version <- parseV
    return $ MaturityVersion maturity version

-- instance Monad VNum where
    -- return x = stringToVC x
    -- x >>= f = f $ x

main :: IO ()
main = do
    case parseOnly stringToVC ( BS.pack "12" ) of 
        Right x -> putStrLn $ show x 
        Left e -> putStrLn $ show e 
    let vn1 = ( Version ( Placeholder ) ( Version ( Placeholder ) ( VersionCompound ( Number 1 ) ) ) )
    putStrLn $ vnToString $ vn1 
    putStrLn $ vnToString $ generateNext vn1
    let vn2 = ( Version ( Placeholder ) ( VersionCompound ( Number 1 ) ) )
    putStrLn $ vnToString $ vn2
    putStrLn $ vnToString $ generateNext vn2
    case parseOnly parseV ("x.1.4.4") of 
        Right x -> putStrLn $ (vnToString x) ++ ": " ++ show x
        Left x -> putStrLn $ show x
    case parseOnly parseMV ("Prod/x.1.x.5") of 
        Right x -> putStrLn $ ( mvToString x ) ++ ": " ++ ( show x )
        Left x -> putStrLn $   show x 