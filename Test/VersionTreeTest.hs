module VersionTreeTest where 

import Test.HUnit
import Version
-- import RoseTree
-- import VersionNumber
import VersionTree
import Util
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.AssertError
import System.Exit

versionTreeTests = test [ 
    {-"test A01"    ~: "vTree2"                                 ~: "{\"children\":[{\"children\":[],\"value\":\"1\"}],\"value\":\"x\"}"        ~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON vTree2 ),-}
    {-"test A02"    ~: "vTree4"                                 ~: "{\"children\":[{\"children\":[],\"value\":\"1\"},{\"children\":[],\"value\":\"2\"},{\"children\":[],\"value\":\"3\"},{\"children\":[],\"value\":\"4\"}],\"value\":\"x\"}"        ~=? ( BS.unpack $ lazyToStrictBS $ JSON.encode $ JSON.toJSON vTree4 ),-}
--    "test A03"    ~: "JSON decode"     ~: Just( RoseTree (Version (VersionCompound NumberPlaceholder)) [RoseTree ( Version (VersionCompound (Number 1))) [] ]) ~=? ((JSON.decode $ BSL.pack"{\"children\":[{\"children\":[],\"value\":{\"version\":\"1\"}}],\"value\":{\"version\":\"x\"}}") :: Maybe (RoseTree Version)),
    "test _"    ~: "empty test"                                ~: True                                                        ~=? True
    ]

runTests :: IO Counts
runTests = do
    results <- runTestTT versionTreeTests
    if (errors results + failures results == 0)
      then
        exitWith ExitSuccess
      else
        exitWith (ExitFailure 1)
