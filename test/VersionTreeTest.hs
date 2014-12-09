module VersionTreeTest where 

import Test.HUnit
import Version
import VersionTree
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Test.AssertError

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS x = BS.concat $ LBS.toChunks x

tests = test [ 
	"test A01"	~: "vTree2" 								~: "{\"children\":[{\"children\":[],\"value\":{\"version\":\"1\"}}],\"value\":{\"version\":\"x\"}}"		~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON vTree2 ),
	"test A02"	~: "vTree4" 								~: "{\"children\":[{\"children\":[],\"value\":{\"version\":\"1\"}},{\"children\":[],\"value\":{\"version\":\"2\"}},{\"children\":[],\"value\":{\"version\":\"3\"}},{\"children\":[],\"value\":{\"version\":\"4\"}}],\"value\":{\"version\":\"x\"}}"		~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON vTree4 ),
	"test _"	~: "empty test"								~: True														~=? True
	]

runTests :: IO Counts
runTests = do
	runTestTT tests