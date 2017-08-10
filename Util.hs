module Util where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

strictBStoLazy :: BS.ByteString -> LBS.ByteString
strictBStoLazy x = LBS.fromStrict x

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs
