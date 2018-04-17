{-#LANGUAGE FlexibleContexts #-}
module Util where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Lens
import Data.List 
import Data.Char (isSpace)
import Data.Time.Clock.POSIX 
import Data.Time.Format

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

strictBStoLazy :: BS.ByteString -> LBS.ByteString
strictBStoLazy x = LBS.fromStrict x

{-replaceNth n newVal (x:xs)-}
     {-| n == 0 = newVal:xs-}
     {-| n > (length (x:xs)) = (x:xs)-}
     {-| otherwise = x:replaceNth (n-1) newVal xs-}
{-replaceNth n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls-}
replaceNth n newval xs = (element n .~ newval) xs

lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs

trim = dropWhileEnd isSpace . dropWhile isSpace

timestampToString :: POSIXTime -> String
timestampToString ptime = formatTime defaultTimeLocale "%s" (posixSecondsToUTCTime ptime)
