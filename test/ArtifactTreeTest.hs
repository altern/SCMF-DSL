module ArtifactTreeTest where 

import Test.HUnit
import Data.Tree
import VersionNumber
import MaturityLevel
import Document
import Artifact
import ArtifactTree
import Version
import VersionTree
import RoseTree
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Test.AssertError
import Util 
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BLS


testArtifactTree1 :: RoseTreeArtifact
testArtifactTree1 = RoseTree ( liftSnapshot $ (Snapshot 1398980989 (Version ( VersionCompound (Number 0) ) ) ( liftDocument $ Document "" "") ) ) []

testSArtifactTree1 :: StringTree
testSArtifactTree1 = Node "0" []

testVArtifactTree1 :: VersionTree
testVArtifactTree1 = RoseTree (Version $ VersionCompound $ Number 0) []

testArtifactTree2 :: RoseTreeArtifact
testArtifactTree2 = RoseTree ( liftSnapshot $ ( Snapshot 1398980989 (Version ( VersionCompound (Number 0) ) ) ( liftDocument $ Document "" "") ) ) 
    [ RoseTree (liftBranch $ (Branch "trunk" (Version (VersionCompound NumberPlaceholder) ) ( liftDocument $ Document "document1" "") ) ) [] ]

testSArtifactTree2 :: StringTree
testSArtifactTree2 = Node "0" [ Node "trunk (x)" [] ]

testVArtifactTree2 :: VersionTree
testVArtifactTree2 = RoseTree (Version $ VersionCompound $ Number 0) [ RoseTree (Version $ VersionCompound $ NumberPlaceholder) [] ]

testArtifactTree3 :: RoseTreeArtifact
testArtifactTree3 = RoseTree (liftSnapshot $ Snapshot 1398980989 (Version $ VersionCompound $ Number 0 ) (liftDocument $ Document "" "") ) 
    [ RoseTree (liftBranch $ Branch "trunk" (Version $ VersionCompound $ NumberPlaceholder ) ( liftDocument $ Document "document1" "latest_content") ) [ 
        RoseTree (liftSnapshot $ Snapshot 1398980990 (Version $ VersionCompound $ Number 1) ( liftDocument $ Document "document1" "content1") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (Version $ VersionCompound $ Number 2) ( liftDocument $ Document "document1" "content2") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980992 (Version $ VersionCompound $ Number 3) ( liftDocument $ Document "document1" "content3") ) [
            RoseTree (liftBranch $ Branch "branch1" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch1") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980993 (Version $ VersionCompound $ Number 4) ( liftDocument $ Document "document1" "content4") ) [], 
                RoseTree (liftSnapshot $ Snapshot 1398980995 (Version $ VersionCompound $ Number 6) ( liftDocument $ Document "document1" "content6") ) [] 
            ],
            RoseTree (liftBranch $ Branch "branch2" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch2") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980999 (Version $ VersionCompound $ Number 10) ( liftDocument $ Document "document1" "content10") ) [] 
            ]
        ],
        RoseTree (liftSnapshot $ Snapshot 1398980994 (Version $ VersionCompound $ Number 5) ( liftDocument $ Document "document1" "content5") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980996 (Version $ VersionCompound $ Number 7) ( liftDocument $ Document "document1" "content7") ) [
            RoseTree (liftBranch $ Branch "branch3" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch3") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980998 (Version $ VersionCompound $ Number 9) ( liftDocument $ Document "document1" "content9") ) [] 
            ]
        ], 
        RoseTree (liftSnapshot $ Snapshot 1398980997 (Version $ VersionCompound $ Number 8) ( liftDocument $ Document "document1" "content8") ) []
    ] ]

testSArtifactTree3 :: StringTree
testSArtifactTree3 = Node "0" [ Node "trunk (x)" 
    [
        Node "1" [],
        Node "2" [],
        Node "3" [
            Node "branch1 (x)" [
                Node "4" [],
                Node "6" []
            ], 
            Node "branch2 (x)" [
                Node "10" []
            ] 
        ],
        Node "5" [],
        Node "7" [
            Node "branch3 (x)" [
                Node "9" []
            ]
        ],
        Node "8" []
    ] ]
	
testVArtifactTree3 :: VersionTree
testVArtifactTree3 = RoseTree (Version $ VersionCompound $ Number 0) [ RoseTree (Version $ VersionCompound $ NumberPlaceholder) 
    [
        RoseTree (Version $ VersionCompound $ Number 1) [],
        RoseTree (Version $ VersionCompound $ Number 2) [],
        RoseTree (Version $ VersionCompound $ Number 3) [
            RoseTree (Version $ VersionCompound $ NumberPlaceholder) [
                RoseTree (Version $ VersionCompound $ Number 4) [],
                RoseTree (Version $ VersionCompound $ Number 6) []
            ], 
            RoseTree (Version $ VersionCompound $ NumberPlaceholder) [
                RoseTree (Version $ VersionCompound $ Number 10) []
            ] 
        ],
        RoseTree (Version $ VersionCompound $ Number 5) [],
        RoseTree (Version $ VersionCompound $ Number 7) [
            RoseTree (Version $ VersionCompound $ NumberPlaceholder) [
                RoseTree (Version $ VersionCompound $ Number 9) []
            ]
        ],
        RoseTree (Version $ VersionCompound $ Number 8) []
    ] ]

tests = test [ 
	"test A01"	~: "artifactTreeToStringTree testArtifactTree1"							~: testSArtifactTree1								~=? (artifactTreeToStringTree testArtifactTree1),
	"test A02"	~: "artifactTreeToStringTree testArtifactTree2"							~: testSArtifactTree2								~=? (artifactTreeToStringTree testArtifactTree2),
	"test A03"	~: "artifactTreeToStringTree testArtifactTree3"							~: testSArtifactTree3								~=? (artifactTreeToStringTree testArtifactTree3),
	"test B01"	~: "artifactTreeToVersionTree testArtifactTree1"						~: testVArtifactTree1								~=? (artifactTreeToVersionTree testArtifactTree1),
	"test B02"	~: "artifactTreeToVersionTree testArtifactTree2"						~: testVArtifactTree2								~=? (artifactTreeToVersionTree testArtifactTree2),
	"test B03"	~: "artifactTreeToVersionTree testArtifactTree3"						~: testVArtifactTree3								~=? (artifactTreeToVersionTree testArtifactTree3),
	"test C01"	~: "findTimestampOfLatestSnapshot testArtifactTree1"					~: 1398980989										~=? (findTimestampOfLatestSnapshot testArtifactTree1),
	"test C02"	~: "findTimestampOfLatestSnapshot testArtifactTree2"					~: 1398980989										~=? (findTimestampOfLatestSnapshot testArtifactTree2),
	"test C03"	~: "findTimestampOfLatestSnapshot testArtifactTree3"					~: 1398980999										~=? (findTimestampOfLatestSnapshot testArtifactTree3),
	"test _"	~: "empty test"								~: True														~=? True
	]

runTests :: IO Counts
runTests = do
	runTestTT tests