module RepositoryTest where 

import Test.HUnit
import Repository
import RoseTree
import VersionNumber
import MaturityLevel
import Version
import Util
import Data.Maybe
import Test.AssertError

defaultTimestamp :: Timestamp
defaultTimestamp = "2"

initialRepositoryWithSupportBranch :: Repository
initialRepositoryWithSupportBranch = RoseTree {value = RepositoryNode {version = Version (VersionNumber [Nothing,Nothing,Just 0]), content = "", timestamp = "0"}, children = [RoseTree {value = RepositoryNode {version = Version (VersionNumber [Nothing,Nothing,Nothing]), content = "", timestamp = "1"}, children = [RoseTree {value = RepositoryNode {version = Version (VersionNumber [Nothing,Nothing,Just 1]), content = "", timestamp = defaultTimestamp}, children = [RoseTree {value = RepositoryNode {version = Version (VersionNumber [Just 0,Nothing,Nothing]), content = "", timestamp = defaultTimestamp}, children = []}]}]}]}

-- newSupportBranch :: Version -> Repository -> Timestamp -> Repository

repositoryTests = test [ 
    "test A01"  ~: "newSupportBranch initialRepository"        ~: (newSupportBranch (stringToVersion "x") initialRepository defaultTimestamp) ~=? initialRepositoryWithSupportBranch,
    "test _"    ~: "empty test"                                ~: True                                                        ~=? True
    ]

runTests :: IO Counts
runTests = do
    runTestTT repositoryTests
