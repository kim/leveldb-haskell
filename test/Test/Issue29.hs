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
    , testCase "Iterator (non-bracketed at all)" test_iterator_nobracket_atall
    , testCase "Snapshot (bracketed)" test_snapshot_bracket
    , testCase "Snapshot (non-bracketed)" test_snapshot_nobracket
    , testCase "Snapshot (withSnapshot)"  test_snapshot_withSnapshot
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

test_iterator_nobracket_atall :: Assertion
test_iterator_nobracket_atall = do
    dir <- tmpDir
    db  <- open dir defaultOptions { createIfMissing = True }
    itr <- createIter db defaultReadOptions { fillCache = False }
    performMajorGC
    threadDelay 100000
    releaseIter itr
    assertBool "program terminated early" True

test_snapshot_bracket :: Assertion
test_snapshot_bracket = do
    dir <- tmpDir
    withDB dir defaultOptions { createIfMissing = True } $ \db -> do
        snap <- createSnapshot db
        performMajorGC
        threadDelay 100000
        putStrLn "releasing snapshot"
        releaseSnapshot db snap
        assertBool "program terminated early" True

test_snapshot_nobracket :: Assertion
test_snapshot_nobracket = do
    dir  <- tmpDir
    db   <- open dir defaultOptions { createIfMissing = True }
    snap <- createSnapshot db
    performMajorGC
    threadDelay 100000
    releaseSnapshot db snap
    assertBool "program terminated early" True

test_snapshot_withSnapshot :: Assertion
test_snapshot_withSnapshot = do
    dir  <- tmpDir
    db   <- open dir defaultOptions { createIfMissing = True }
    withSnapshot db $ \_ -> do
        performMajorGC
        threadDelay 100000
        assertBool "program terminated early" True


tmpDir :: IO FilePath
tmpDir = getTemporaryDirectory >>= flip createTempDirectory "leveldb-issue29."
