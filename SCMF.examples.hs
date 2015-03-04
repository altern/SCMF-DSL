module SCMF_Examples where

import Data.Tree
import Data.Tree.Pretty
import ArtifactTree
import Document
import Artifact
import RoseTree
import Version
import VersionNumber
import VersionTree
import Platform


artifactTree1 :: RoseTreeArtifact
artifactTree1 = RoseTree ( liftSnapshot $ (Snapshot 1398980989 (Version ( VersionCompound (Number 0) ) ) ( liftDocument $ Document "" "") ) ) []

sArtifactTree1 :: StringTree
sArtifactTree1 = Node "0" []

artifactTree2 :: RoseTreeArtifact
artifactTree2 = RoseTree ( liftSnapshot $ ( Snapshot 1398980989 (Version ( VersionCompound (Number 0) ) ) ( liftDocument $ Document "" "") ) ) 
    [ RoseTree (liftBranch $ (Branch "trunk" (Version (VersionCompound NumberPlaceholder) ) ( liftDocument $ Document "document1" "") ) ) [] ]

sArtifactTree2 :: StringTree
sArtifactTree2 = Node "0" [ Node "trunk (x)" [] ]
    
artifactTree3 :: RoseTreeArtifact
artifactTree3 = RoseTree (liftSnapshot $ Snapshot 1398980989 (Version $ VersionCompound $ Number 0 ) (liftDocument $ Document "" "") ) 
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

t = artifactTree3

a1 = liftSnapshot $ Snapshot 1398980990 (Version $ VersionCompound $ Number 1) ( liftDocument $ Document "document1" "content1") 
a5 = liftSnapshot $ Snapshot 1398980994 (Version $ VersionCompound $ Number 5) ( liftDocument $ Document "document1" "content5") 
a6 = liftSnapshot $ Snapshot 1398980995 (Version $ VersionCompound $ Number 6) ( liftDocument $ Document "document1" "content6")
br1 = liftBranch $ Branch "branch1" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch1") 
list = [ RoseTree a1 [], 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (Version $ VersionCompound $ Number 2) ( liftDocument $ Document "document1" "content2") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980992 (Version $ VersionCompound $ Number 3) ( liftDocument $ Document "document1" "content3") ) [] ]
subtree = RoseTree (liftBranch $ Branch "trunk" (Version $ VersionCompound $ NumberPlaceholder ) ( liftDocument $ Document "document1" "latest_content") ) [ 
        RoseTree (liftSnapshot $ Snapshot 1398980990 (Version $ VersionCompound $ Number 1) ( liftDocument $ Document "document1" "content1") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (Version $ VersionCompound $ Number 2) ( liftDocument $ Document "document1" "content2") ) [], 
        RoseTree (liftSnapshot $ Snapshot 1398980992 (Version $ VersionCompound $ Number 3) ( liftDocument $ Document "document1" "content3") ) [
            RoseTree (liftBranch $ Branch "branch1" (Version $ VersionCompound $ NumberPlaceholder) ( liftDocument $ Document "document1" "content_branch1") ) [
                RoseTree (liftSnapshot $ Snapshot 1398980993 (Version $ VersionCompound $ Number 4) ( liftDocument $ Document "document1" "content4") ) [], 
                RoseTree (liftSnapshot $ Snapshot 1398980995 (Version $ VersionCompound $ Number 6) ( liftDocument $ Document "document1" "content6") ) [] 
            ]
        ]
    ]

artifactTree4 :: RoseTreeArtifact 
artifactTree4 = RoseTree (liftBranch $ Branch "trunk" (stringToVersion "x.x.x") (liftDocument $ Document "" "")) [
        RoseTree (liftSnapshot $ Snapshot 1398980990 (stringToVersion "x.x.0") (liftDocument $ Document "" "")) [
                RoseTree (liftBranch $ Branch "1.x" (stringToVersion "x.1.x") (liftDocument $ Document "doc1" "release1")) []
        ] , 
        RoseTree (liftSnapshot $ Snapshot 1398980991 (stringToVersion "x.x.1") (liftDocument $ Document "" "")) [
                RoseTree (liftBranch $ Branch "2.x" (stringToVersion "x.2.x") (liftDocument $ Document "doc1" "release2")) [
                        RoseTree (liftSnapshot $ Snapshot 1398980992 (stringToVersion "x.2.0") (liftDocument $ Document "doc1" "release2.0")) []
                ]
        ] , 
        RoseTree (liftSnapshot $ Snapshot 1398980993 (stringToVersion "x.x.2") (liftDocument $ Document "" "")) [
                RoseTree (liftBranch $ Branch "1.x.x" (stringToVersion "1.x.x") (liftDocument $ Document "doc1" "support1")) [
                        RoseTree (liftSnapshot $ Snapshot 1398980994 (stringToVersion "1.x.0") (liftDocument $ Document "doc1" "support1_0")) []
                ]
        ] ]
sArtifactTree3 :: StringTree
sArtifactTree3 = Node "0" [ Node "trunk (x)" 
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


platform1 = Platform "platform1" (PlatformContents (liftDocument emptyDocument) (initialVersion(NumberPlaceholder) ) "")
platform2 = Platform "platform2" (PlatformContents (liftDocument emptyDocument) (initialVersion(NumberPlaceholder) ) "")
platform3 = Platform "platform3" (PlatformContents (liftDocument emptyDocument) (initialVersion(NumberPlaceholder) ) "")

platformDB = [ platform1, platform2, platform3 ]

deploymentRule1 = DeploymentRuleVersion ( Version ( VersionCompound $ Number 10 ) ) "platform1"
deploymentRule2 = DeploymentRuleArtifact ( liftBranch $ Branch "trunk" (Version $ VersionCompound $ NumberPlaceholder) (liftDocument emptyDocument) ) "platform2"
deploymentRule3 = DeploymentRuleArtifact ( liftBranch $ Branch "branch1" (Version $ VersionCompound $ NumberPlaceholder ) (liftDocument emptyDocument) ) "platform3"
deploymentRule4 = DeploymentRuleArtifact ( liftBranch $ Branch "trunk" (Version $ VersionCompound $ NumberPlaceholder ) (liftDocument emptyDocument) ) "platform3"

deploymentRules = [deploymentRule1, deploymentRule2, deploymentRule3, deploymentRule4]

displayPlatformsForArtifactTree :: ArtifactTree -> IO ()
displayPlatformsForArtifactTree aTree = putStrLn $ drawVerticalTree ( artifactListToPlatformsTree ( aTree ) (deploy aTree deploymentRules platformDB ) )
