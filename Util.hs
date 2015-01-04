module Util where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

strictBStoLazy :: BS.ByteString -> LBS.ByteString
strictBStoLazy x = LBS.fromStrict x