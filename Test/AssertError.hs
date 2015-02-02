module Test.AssertError where

import Control.Exception
import Control.Monad
import Test.HUnit

instance Eq ErrorCall where
    x == y = (show x) == (show y)

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex) 

assertError ex f = 
    TestCase $ assertException (ErrorCall ex) $ evaluate f