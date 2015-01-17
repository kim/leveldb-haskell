{-# LANGUAGE OverloadedStrings #-}
-- |
-- Comprehensive walkthough of the functionality provided by this library.
--
module Main where

import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (release)
import           Data.ByteString.Char8        hiding (take)
import           Data.Default
import           Data.Monoid
import           Database.LevelDB
import qualified Database.LevelDB.Streaming   as S
import           Prelude                      hiding (putStrLn)


main :: IO ()
main = runResourceT $ do
    printVersion

    db <- open "/tmp/leveltest"
               defaultOptions{ createIfMissing = True
                             , cacheSize= 2048
                             }

    putStrLn' "Put value"
    put db def "foo" "bar"
    get db def "foo" >>= liftIO . print

    putStrLn' ""

    putStrLn' "Delete value"
    delete db def "foo"
    get db def "foo" >>= liftIO . print


    putStrLn' ""

    (releaseSnap, snap) <- createSnapshot' db

    putStrLn' "Write batch"
    write db def{sync = True} [ Put "a" "one"
                              , Put "b" "two"
                              , Put "c" "three"
                              ]

    putStrLn' "Dump entries with old snapshot"
    withIterator db def{useSnapshot = Just snap} $ \iter -> dumpEntries iter

    -- early release snapshot
    release releaseSnap

    -- here, we keep the iterator around for later reuse.
    -- Note that we don't explicitly release it (and thus don't keep the release
    -- key). The iterator will be released when runResourceT terminates.
    iter <- iterOpen db def
    putStrLn' "Dump entries with fresh iterator"
    dumpEntries iter

    putStrLn' ""

    printDbSize db
    putStrLn' "Trigger compaction"
    compactRange db ("a", "z")
    printDbSize db


    putStrLn' ""

    putStrLn' "  BEGIN dump properties"
    getProperty db SSTables >>= printProperty "SSTables:"
    getProperty db Stats >>= printProperty "Stats:"
    getProperty db (NumFilesAtLevel 1) >>= printProperty "Num files at level 1:"
    putStrLn' "  END   dump properties"


    putStrLn' ""

    putStrLn' "Delete batch"
    write db def [ Del "a"
                 , Del "b"
                 , Del "c"
                 ]

    putStrLn' "Dump entries"
    dumpEntries iter

    return ()

  where
    dumpEntries iter = liftIO $
            S.toList (S.entrySlice iter S.AllKeys S.Asc)
        >>= print

    printProperty l p = do
        putStrLn' l
        maybe (putStrLn' "n/a") putStrLn' p

    printVersion = do
        v <- versionBS
        putStrLn' $ "LevelDB Version: " <> v

    printDbSize db = do
        s <- approximateSize db ("a", "z")
        putStrLn' $ "Approximate DB size: " <> pack (show s)

    versionBS = do
        (major, minor) <- version
        return $ intToBs major <> "." <> intToBs minor

    intToBs :: Int -> ByteString
    intToBs = pack . show

    putStrLn' = liftIO . putStrLn
