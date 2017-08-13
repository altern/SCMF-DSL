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
    "test A01"    ~: "JSON.toJSON artifact1"                            ~: "{\"snapshot\":{\"version\":{\"version\":\"Dev/10\"},\"artifact\":{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}},\"timestamp\":12372}}"                                ~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON artifact1 ),
    "test A02"    ~: "JSON.toJSON artifact2"                            ~: "{\"branch\":{\"version\":{\"version\":\"x\"},\"name\":\"branch3\",\"artifact\":{\"document\":{\"name\":\"doc2\",\"content\":\"content2\"}}}}"                                    ~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON artifact2 ),
    "test B01"    ~: "JSON.FromJSON artifact1"                        ~: Just (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) )                                    ~=? ( JSON.decode $ BLS.pack "{\"snapshot\":{\"version\":{\"version\":\"Dev/10\"},\"artifact\":{\"document\":{\"name\":\"doc1\",\"content\":\"content1\"}},\"timestamp\":12372}}" :: Maybe Snapshot ),
    "test B02"    ~: "JSON.FromJSON artifact1"                        ~: Just (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) )                                            ~=? ( JSON.decode $ BLS.pack "{\"branch\":{\"version\":{\"version\":\"x\"},\"name\":\"branch3\",\"artifact\":{\"document\":{\"name\":\"doc2\",\"content\":\"content2\"}}}}" :: Maybe Branch ),
    "test C01"    ~: "toString artifact1"                        ~: "Dev/10"                                                        ~=? toString (Artifact ( Right (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ), 
    "test C02"    ~: "toString artifact2"                        ~: "branch3 (x)"                                                ~=? toString (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc2" "content2" ) ) ) ) ) ), 
    "test D01"    ~: "artifactToVersion artifact1"                    ~: (MaturityVersion Dev $ VersionNumber [Just 10])         ~=? artifactToVersion (Artifact ( Right (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ), 
    "test D02"    ~: "artifactToVersion artifact2"                    ~: (Version $ VersionNumber [Nothing])                ~=? artifactToVersion (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc2" "content2" ) ) ) ) ) ), 
    "test E01"    ~: "artifactToDocument artifact1"                    ~: DocumentOrDirectory (Left (Document "doc1" "content1" ) )     ~=? artifactToDocument (Artifact ( Right (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ), 
    "test E02"    ~: "artifactToDocument artifact2"                    ~: DocumentOrDirectory (Left (Document "doc2" "content2" ) )    ~=? artifactToDocument (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc2" "content2" ) ) ) ) ) ), 
    "test F01"    ~: "getArtifactTimestamp artifact1"                    ~: 12372         ~=? getArtifactTimestamp (Artifact ( Right (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ), 
    "test F02"    ~: "getArtifactTimestamp artifact2"                    ~: 0            ~=? getArtifactTimestamp (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ), 
    "test G01"    ~: "getArtifactName artifact1"                        ~: ""             ~=? getArtifactName (Artifact ( Right (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ), 
    "test G02"    ~: "getArtifactName artifact2"                        ~: "branch3"    ~=? getArtifactName (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ), 
    "test H01"    ~: "artifactHasVersion artifact1 Dev/10"            ~: True         ~=? artifactHasVersion (Artifact ( Right (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ) (MaturityVersion Dev $ VersionNumber [Just 10]), 
    "test H02"    ~: "artifactHasVersion artifact2 x"                    ~: True            ~=? artifactHasVersion (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ) (Version $ VersionNumber [Nothing]), 
    "test H03"    ~: "artifactHasVersion artifact2 Dev/10"            ~: False        ~=? artifactHasVersion (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ) (MaturityVersion Dev $ VersionNumber [Just 10] ), 
    "test I01"    ~: "detectAllowedChanges artifact1"                    ~: None         ~=? detectAllowedChanges (Artifact ( Right (Snapshot 12372 ( MaturityVersion Dev $ VersionNumber [Just 10] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ) , 
    "test I02"    ~: "detectAllowedChanges artifact2 "                ~: Any            ~=? detectAllowedChanges (Artifact ( Left (Branch "branch3" ( Version $ VersionNumber [Nothing] ) ( DocumentOrDirectory (Left (Document "doc1" "content1" ) ) ) ) ) ) , 
    "test _"    ~: "empty test"                                ~: True                                                        ~=? True
    ]

runTests :: IO Counts
runTests = do
    runTestTT tests
