module DocumentTest where 

import Test.HUnit
import Document
import Util
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BLS

import Test.AssertError

documentTests = test [ 
	"test A01"	~: "JSON.toJSON doc1"							~: "{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}}"									~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON doc1 ),
	"test A02"	~: "JSON.toJSON dir1"							~: "{\"directory\":{\"name\":\"dir1\",\"content\":[{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}}]}}"									~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON dir1 ),
	"test B01"	~: "JSON.FromJSON jsonDoc1"						~: Just (Document "doc1" "content1")												~=? ( JSON.decode $ BLS.pack "{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}}" :: Maybe Document),
	--"test B02"	~: "JSON.FromJSON jsonDir1"					~: Just (Directory "dir1" [DocumentOrDirectory (Left (Document "doc1" "content1"))])	~=? ( JSON.decode $ BLS.pack "{\"directory\":{\"name\":\"dir1\",\"content\":[{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}}]}}" :: Maybe Directory),
	"test _"	~: "empty test"								~: True														~=? True
	]

runTests :: IO Counts
runTests = do
	runTestTT documentTests
