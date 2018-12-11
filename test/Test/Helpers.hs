module Test.Helpers
    ( TestDB
    , withTestDB
    , initTestDB
    , destroyTestDB
    , testDBHandle
    )
where

import           Control.Monad.Catch       (bracket, finally)
import qualified Database.LevelDB.Base     as L
import           Database.LevelDB.Internal (unsafeClose)
import           System.Directory          (getTemporaryDirectory)
import           System.IO.Temp            (createTempDirectory)

data TestDB = TestDB !L.DB FilePath

withTestDB :: L.Options -> (TestDB -> IO a) -> IO a
withTestDB opts = bracket (initTestDB opts) destroyTestDB

initTestDB :: L.Options -> IO TestDB
initTestDB opts = do
    tmp <- getTemporaryDirectory
    dir <- createTempDirectory tmp "leveldb-tests"
    db  <- L.open dir opts { L.createIfMissing = True, L.errorIfExists = True }
    pure $ TestDB db dir

destroyTestDB :: TestDB -> IO ()
destroyTestDB (TestDB db dir) =
    unsafeClose db `finally` L.destroy dir L.defaultOptions

testDBHandle :: TestDB -> L.DB
testDBHandle (TestDB db _) = db
