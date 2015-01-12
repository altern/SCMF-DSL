module ArtifactTest where 

import Test.HUnit
import VersionNumber
import MaturityLevel
import Document
import Artifact
import Version
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Test.AssertError
import Util 
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BLS

tests = test [ 
	"test A01"	~: "JSON.toJSON artifact1"							~: "{\"snapshot\":{\"version\":{\"version\":\"Dev/10\"},\"artifact\":{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}},\"timestamp\":12372}}"								~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON artifact1 ),
	"test A02"	~: "JSON.toJSON artifact2"							~: "{\"branch\":{\"version\":{\"version\":\"x\"},\"name\":\"branch3\",\"artifact\":{\"document\":{\"name\":\"doc2\",\"content\":\"content2\"}}}}"									~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON artifact2 ),
	"test B01"	~: "JSON.FromJSON artifact1"						~: Just (Snapshot 12372 ( MaturityVersion Dev ( VersionCompound ( Number 10 ) ) ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) )									~=? ( JSON.decode $ BLS.pack "{\"snapshot\":{\"version\":{\"version\":\"Dev/10\"},\"artifact\":{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}},\"timestamp\":12372}}" :: Maybe Snapshot ),
	"test B02"	~: "JSON.FromJSON artifact1"						~: Just (Branch "branch3" ( Version ( VersionCompound NumberPlaceholder ) ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) )											~=? ( JSON.decode $ BLS.pack "{\"branch\":{\"version\":{\"version\":\"x\"},\"name\":\"branch3\",\"artifact\":{\"document\":{\"name\":\"doc2\",\"content\":\"content2\"}}}}" :: Maybe Branch ),
	"test _"	~: "empty test"								~: True														~=? True
	]

runTests :: IO Counts
runTests = do
	runTestTT tests