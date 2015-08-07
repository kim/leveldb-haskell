module Test.Issue29 (tests) where

import Control.Concurrent
import Database.LevelDB.Base
import System.Directory
import System.IO.Temp
import System.Mem
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Issue29"
    [ testCase "Iterator (bracketed)"     test_iterator_bracket
    , testCase "Iterator (non-bracketed)" test_iterator_nobracket
    ]

test_iterator_nobracket :: Assertion
test_iterator_nobracket = do
    dir <- tmpDir
    db  <- open dir defaultOptions { createIfMissing = True }
    withIter db defaultReadOptions { fillCache = False } $ \_ -> do
        performMajorGC
        threadDelay 100000
        assertBool "program terminated early" True

test_iterator_bracket :: Assertion
test_iterator_bracket = do
    dir <- tmpDir
    withDB dir defaultOptions { createIfMissing = True } $ \db ->
        withIter db defaultReadOptions { fillCache = False } $ \_ -> do
            performMajorGC
            threadDelay 100000
            assertBool "program terminated early" True

tmpDir :: IO FilePath
tmpDir = getTemporaryDirectory >>= flip createTempDirectory "leveldb-issue29."
