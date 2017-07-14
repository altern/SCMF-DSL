import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Runners.Console
import Data.Monoid
import Control.Monad
import ArtifactTreeTest
import VersionNumberTest
import VersionTest
import VersionTreeTest
import DocumentTest

{- main :: IO ()
main = defaultMainWithOpts
       [
        testCase "version number test" ( versionNumberTests)
       ,testCase "artifact tree test" ( artifactTreeTests)	
       ]
       mempty -}

main :: IO Counts
main = runTestTT $ TestList [versionNumberTests, artifactTreeTests, versionTests, versionTreeTests]
