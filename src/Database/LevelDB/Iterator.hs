-- |
-- Module      : Database.LevelDB.Iterator
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
-- Iterating over key ranges.
--

module Database.LevelDB.Iterator
    ( Iterator
    , createIter
    , iterEntry
    , iterFirst
    , iterGetError
    , iterKey
    , iterLast
    , iterNext
    , iterPrev
    , iterSeek
    , iterValid
    , iterValue
    , releaseIter
    , withIter
    )
where

import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad             (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Data.ByteString           (ByteString)
import           Foreign
import           Foreign.C.Error           (throwErrnoIfNull)
import           Foreign.C.String          (CString, peekCString)
import           Foreign.C.Types           (CSize)

import           Database.LevelDB.C
import           Database.LevelDB.Internal
import           Database.LevelDB.Types

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Unsafe    as BU

-- | Iterator handle
--
-- Note that an 'Iterator' requires external synchronization if it is shared
-- between multiple threads which mutate it's state. See
-- @examples/iterforkio.hs@ for a simple example of how to do that.
data Iterator = Iterator !IteratorPtr !ReadOptionsPtr deriving (Eq)

-- | Create an 'Iterator'.
--
-- The iterator should be released with 'releaseIter'.
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
createIter :: MonadIO m => DB -> ReadOptions -> m Iterator
createIter (DB db_ptr _ _) opts = liftIO $ do
    opts_ptr <- mkCReadOpts opts
    flip onException (freeCReadOpts opts_ptr) $ do
        iter_ptr <- throwErrnoIfNull "create_iterator" $
                        c_leveldb_create_iterator db_ptr opts_ptr
        return $ Iterator iter_ptr opts_ptr

-- | Release an 'Iterator'.
--
-- The handle will be invalid after calling this action and should no
-- longer be used. Calling this function with an already released 'Iterator'
-- will cause a double-free error!
releaseIter :: MonadIO m => Iterator -> m ()
releaseIter (Iterator iter_ptr opts) = liftIO $
    c_leveldb_iter_destroy iter_ptr `finally` freeCReadOpts opts

-- | Run an action with an 'Iterator'
withIter :: (MonadMask m, MonadIO m) => DB -> ReadOptions -> (Iterator -> m a) -> m a
withIter db opts = bracket (createIter db opts) releaseIter

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: MonadIO m => Iterator -> m Bool
iterValid (Iterator iter_ptr _) = liftIO $ do
    x <- c_leveldb_iter_valid iter_ptr
    return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: MonadIO m => Iterator -> ByteString -> m ()
iterSeek (Iterator iter_ptr _) key = liftIO $
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        c_leveldb_iter_seek iter_ptr key_ptr (intToCSize klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: MonadIO m => Iterator -> m ()
iterFirst (Iterator iter_ptr _) = liftIO $ c_leveldb_iter_seek_to_first iter_ptr

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: MonadIO m => Iterator -> m ()
iterLast (Iterator iter_ptr _) = liftIO $ c_leveldb_iter_seek_to_last iter_ptr

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterPrev' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterNext :: MonadIO m => Iterator -> m ()
iterNext (Iterator iter_ptr _) = liftIO $ do
    valid <- c_leveldb_iter_valid iter_ptr
    when (valid /= 0) $ c_leveldb_iter_next iter_ptr

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterNext' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterPrev :: MonadIO m => Iterator -> m ()
iterPrev (Iterator iter_ptr _) = liftIO $ do
    valid <- c_leveldb_iter_valid iter_ptr
    when (valid /= 0) $ c_leveldb_iter_prev iter_ptr

-- | Return the key for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterKey :: MonadIO m => Iterator -> m (Maybe ByteString)
iterKey = liftIO . flip iterString c_leveldb_iter_key

-- | Return the value for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterValue :: MonadIO m => Iterator -> m (Maybe ByteString)
iterValue = liftIO . flip iterString c_leveldb_iter_value

-- | Return the current entry as a pair, if the iterator is currently positioned
-- at an entry, ie. 'iterValid'.
iterEntry :: MonadIO m => Iterator -> m (Maybe (ByteString, ByteString))
iterEntry iter = liftIO $ do
    mkey <- iterKey iter
    mval <- iterValue iter
    return $ (,) <$> mkey <*> mval

-- | Check for errors
--
-- Note that this captures somewhat severe errors such as a corrupted database.
iterGetError :: MonadIO m => Iterator -> m (Maybe ByteString)
iterGetError (Iterator iter_ptr _) = liftIO $
    alloca $ \err_ptr -> do
        poke err_ptr nullPtr
        c_leveldb_iter_get_error iter_ptr err_ptr
        erra <- peek err_ptr
        if erra == nullPtr
            then return Nothing
            else do
                err <- peekCString erra
                return . Just . BC.pack $ err

--
-- Internal
--

iterString :: Iterator
           -> (IteratorPtr -> Ptr CSize -> IO CString)
           -> IO (Maybe ByteString)
iterString (Iterator iter_ptr _) f = do
    valid <- c_leveldb_iter_valid iter_ptr
    if valid == 0
        then return Nothing
        else alloca $ \len_ptr -> do
                 ptr <- f iter_ptr len_ptr
                 if ptr == nullPtr
                     then return Nothing
                     else do
                         len <- peek len_ptr
                         Just <$> BS.packCStringLen (ptr, cSizeToInt len)
