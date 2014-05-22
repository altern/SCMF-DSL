{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Platform where 

import Version
import VersionNumber
import Artifact
import ArtifactTree 
import Data.Tree
import Data.Tree.Pretty
import RoseTree
import Text.Show
import Text.PrettyPrint.HughesPJ

class Pretty a where
    prettyPrint :: a -> IO ()
    prettyPrint = print . prettyShow
    prettyShow :: a -> Doc

pprint :: Pretty a => a -> IO ()
pprint = prettyPrint

instance Pretty Double where
    prettyShow = double
instance Pretty Platform where
    prettyShow (Platform name contents) = prettyShow ( (show name) ++ ": " ++ (show contents) )
instance Pretty a => Pretty [a] where
    prettyShow = brackets . sep . punctuate (text ", ") . map prettyShow
instance (Pretty a,Pretty b) => Pretty (a,b) where
    prettyShow (a,b) = parens (prettyShow a <> text ", " <> prettyShow b)
instance Pretty Char where
    prettyShow = char

data ConnectionType = FTP
                    | LOCAL
                    | SFTP
                    | SSH
                    | RSYNC
                    deriving (Show)

data ConnectionProperty = Host String
                        | Port Int
                        | Username String
                        | Password String
                        | RemoteDirectory String
                        deriving (Show)

data Connection = Connection ConnectionType [ConnectionProperty]
                deriving (Show)

type PlatformName = String

data PlatformContents = PlatformContents DocumentOrDirectory Version BranchName
                deriving (Show)

data Platform = Platform PlatformName PlatformContents -- Connection 

initialPlatform = Platform "" (PlatformContents emptyDocument initialVersion "")

instance Show Platform where
    show (Platform name ( PlatformContents document version branchName) ) = (show name) ++ ": " ++ (show version) ++ " (" ++  branchName ++ ")"

instance Eq Platform where 
    ( Platform nameA _ ) == ( Platform nameB _ ) = ( nameA == nameB )

class GetPlatformName a where
    getPlatformName :: a -> PlatformName

instance GetPlatformName Platform where
    getPlatformName (Platform name _ ) = name

instance GetPlatformName [Platform] where
    getPlatformName [] = ""
    getPlatformName (x:[]) = (getPlatformName x) 
    getPlatformName (x:xs) = (getPlatformName x) ++ ", " ++ (getPlatformName xs)

type PlatformDB = [Platform]

    
data DeploymentRule = DeploymentRuleVersion Version PlatformName
                    | DeploymentRuleArtifact Artifact PlatformName
                    deriving (Show)
                    
type DeploymentRules = [DeploymentRule]
    
class ApplyDeploymentRules a where
    applyDeploymentRules :: DeploymentRules -> a -> Artifact -> a
    applyDeploymentRule :: DeploymentRule -> a -> Artifact -> a

instance ApplyDeploymentRules Platform where 
    applyDeploymentRules [] platform _ = platform
    applyDeploymentRules (x:xs) platform artifact = applyDeploymentRule x (applyDeploymentRules xs platform artifact) artifact
    applyDeploymentRule rule@(DeploymentRuleVersion version platformNameA) platform@(Platform platformNameB _) artifact
        | (platformNameA == platformNameB) && ((getArtifactVersion artifact) == version) = (Platform platformNameA (PlatformContents (getArtifactContents artifact) version (getArtifactName artifact)  ) )
        | otherwise = platform
    applyDeploymentRule rule@(DeploymentRuleArtifact artifactA platformNameA) platform@(Platform platformNameB _) artifactB
        | (platformNameA == platformNameB) && (artifactA == artifactB) = (Platform platformNameA ( PlatformContents (getArtifactContents artifactB) ( getArtifactVersion artifactB ) (getArtifactName artifactB) ) )
        | otherwise = platform
    
instance ApplyDeploymentRules PlatformDB where 
    applyDeploymentRule _ [] _ = []
    applyDeploymentRule rule (x:xs) artifact = [ applyDeploymentRule rule x artifact] ++ (applyDeploymentRule rule xs artifact)
    applyDeploymentRules _ [] artifact = []
    applyDeploymentRules [] db artifact = db
    applyDeploymentRules rules (x:xs) artifact = [applyDeploymentRules rules x artifact] ++ (applyDeploymentRules rules xs artifact)
    
deploy :: ArtifactTree -> DeploymentRules -> PlatformDB -> PlatformDB
deploy aTree _ [] = []
deploy aTree [] db = db
deploy aTree (x:xs) db = applyDeploymentRule x (deploy aTree xs db) (findArtifactByDeploymentRule aTree x) 

findArtifactByDeploymentRule :: ArtifactTree -> DeploymentRule -> Artifact
findArtifactByDeploymentRule aTree (DeploymentRuleVersion version _) = searchArtifactTree aTree version !! 0
findArtifactByDeploymentRule aTree (DeploymentRuleArtifact artifact _) = searchArtifactTree aTree artifact !! 0

-- CONFLICTING RULES --

doRulesConflict :: DeploymentRule -> DeploymentRule -> Bool
doRulesConflict rule1@( DeploymentRuleVersion version1 platformName1 ) rule2@( DeploymentRuleVersion version2 platformName2 ) = (version1 /= version2 && platformName1 == platformName2)
doRulesConflict rule1@( DeploymentRuleArtifact artifact1 platformName1 ) rule2@( DeploymentRuleArtifact artifact2 platformName2 ) = ( (version1 /= version2 || artifactName1 /= artifactName2) && platformName1 == platformName2 ) where 
    version1 = getArtifactVersion artifact1
    version2 = getArtifactVersion artifact2
    artifactName1 = getArtifactName artifact1
    artifactName2 = getArtifactName artifact2
doRulesConflict rule1@( DeploymentRuleArtifact artifact1 platformName1 ) rule2@( DeploymentRuleVersion version2 platformName2 ) = (version1 /= version2 && platformName1 == platformName2) where 
    version1 = getArtifactVersion artifact1
doRulesConflict rule1@( DeploymentRuleVersion version1 platformName1 ) rule2@( DeploymentRuleArtifact artifact2 platformName2 ) = (version1 /= version2 && platformName1 == platformName2) where 
    version2 = getArtifactVersion artifact2

findConflictingRules :: DeploymentRules -> DeploymentRules
findConflictingRules [] = []
findConflictingRules (x:xs) = (detectConflictingDeploymentRules x xs) ++ (findConflictingRules xs)

detectConflictingDeploymentRules :: DeploymentRule -> DeploymentRules -> DeploymentRules
detectConflictingDeploymentRules rule [] = []
detectConflictingDeploymentRules rule (x:xs)  
    | doRulesConflict rule x = [rule, x] ++ (detectConflictingDeploymentRules rule xs) 
    | otherwise = (detectConflictingDeploymentRules rule xs) 

-- ARTIFACT TREE TO DEPLOYMENT PLATFORMS --

artifactListToPlatformsList :: ArtifactTreeList -> PlatformDB -> StringTreeList
artifactListToPlatformsList [] _ = []
artifactListToPlatformsList (x:[]) db = [artifactListToPlatformsTree x db]
artifactListToPlatformsList (x:xs) db = (artifactListToPlatformsTree x db):(artifactListToPlatformsList xs db) 

artifactListToPlatformsTree :: ArtifactTree -> PlatformDB -> StringTree
artifactListToPlatformsTree (RoseTree artifact []) db = Node (show ( getPlatformName ( findPlatformByArtifact artifact db ))) []
artifactListToPlatformsTree (RoseTree artifact (x:[])) db = Node (show ( getPlatformName ( findPlatformByArtifact artifact db ))) [ artifactListToPlatformsTree x db ]
artifactListToPlatformsTree (RoseTree artifact (x:xs)) db = Node (show ( getPlatformName ( findPlatformByArtifact artifact db ))) ( (artifactListToPlatformsTree x db) : (artifactListToPlatformsList xs db) )

class FindPlatformByArtifact a where 
    findPlatformByArtifact :: Artifact -> a -> PlatformDB
    findPlatformByVersion :: Version -> a -> PlatformDB
    
instance FindPlatformByArtifact Platform where
    findPlatformByArtifact artifact platform@(Platform platformName (PlatformContents document version branchName) ) 
        | ( (getArtifactVersion artifact) == version && (getArtifactName artifact) == branchName) = [ platform ]
        | otherwise = []
    findPlatformByVersion version1 platform@(Platform platformName (PlatformContents document version2 branchName) )
        | (version1 == version2) = [ platform ]
        | otherwise = []
    
instance FindPlatformByArtifact PlatformDB where 
    findPlatformByArtifact _ [] = []
    findPlatformByArtifact artifact (x:xs) = (findPlatformByArtifact artifact x) ++ (findPlatformByArtifact artifact xs)
    findPlatformByVersion _ [] = []
    findPlatformByVersion version (x:xs) = (findPlatformByVersion version x) ++ (findPlatformByVersion version xs)

-- EXAMPLES --

platform1 = Platform "platform1" (PlatformContents emptyDocument initialVersion "")
platform2 = Platform "platform2" (PlatformContents emptyDocument initialVersion "")
platform3 = Platform "platform3" (PlatformContents emptyDocument initialVersion "")

platformDB = [ platform1, platform2, platform3 ]

deploymentRule1 = DeploymentRuleVersion ( Version ( Number 10 ) ) "platform1"
deploymentRule2 = DeploymentRuleArtifact ( Branch "trunk" (Version NumberPlaceholder) emptyDocument ) "platform2"
deploymentRule3 = DeploymentRuleArtifact ( Branch "branch1" (Version NumberPlaceholder ) emptyDocument ) "platform3"
deploymentRule4 = DeploymentRuleArtifact ( Branch "trunk" (Version NumberPlaceholder ) emptyDocument ) "platform3"

deploymentRules = [deploymentRule1, deploymentRule2, deploymentRule3, deploymentRule4]

displayPlatformsForArtifactTree :: ArtifactTree -> IO ()
displayPlatformsForArtifactTree aTree = putStrLn $ drawVerticalTree ( artifactListToPlatformsTree ( aTree ) (deploy aTree deploymentRules platformDB ) )