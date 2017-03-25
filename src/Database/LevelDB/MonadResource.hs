-- |
-- Module      : Database.LevelDB.MonadResource
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--

module Database.LevelDB.MonadResource
    ( -- * Exported Types
      DB
    , BatchOp(..)
    , Comparator(..)
    , Compression(..)
    , Options(..)
    , ReadOptions(..)
    , Snapshot
    , WriteBatch
    , WriteOptions(..)
    , Range

    -- * Defaults
    , defaultOptions
    , defaultWriteOptions
    , defaultReadOptions

    -- * Basic Database Manipulation
    , withSnapshot
    , open
    , put
    , delete
    , write
    , get
    , createSnapshot
    , createSnapshot'

    -- * Filter Policy / Bloom Filter
    , FilterPolicy(..)
    , bloomFilter

    -- * Administrative Functions
    , Property(..), getProperty
    , destroy
    , repair
    , approximateSize
    , compactRange
    , version

    -- * Iteration
    , Iterator
    , withIterator
    , iterOpen
    , iterOpen'
    , iterValid
    , iterSeek
    , iterFirst
    , iterLast
    , iterNext
    , iterPrev
    , iterKey
    , iterValue
    , iterGetError

    -- * Re-exports
    , MonadResource (..)
    , runResourceT
    , resourceForkIO
    )
where

import           Control.Applicative          ((<$>))
import           Control.Monad.Trans.Resource
import           Database.LevelDB.Base        (BatchOp, BloomFilter, Comparator,
                                               Compression, DB, FilterPolicy,
                                               Iterator, Options, Property,
                                               Range, ReadOptions, Snapshot,
                                               WriteBatch, WriteOptions,
                                               approximateSize, compactRange,
                                               defaultOptions,
                                               defaultReadOptions,
                                               defaultWriteOptions, delete,
                                               destroy, get, getProperty,
                                               iterFirst, iterGetError, iterKey,
                                               iterLast, iterNext, iterPrev,
                                               iterSeek, iterValid, iterValue,
                                               put, repair, version, write)
import qualified Database.LevelDB.Base        as Base
import qualified Database.LevelDB.Internal    as Internal


-- | Create a 'BloomFilter'
bloomFilter :: MonadResource m => Int -> m BloomFilter
bloomFilter i =
    snd <$> allocate (Base.createBloomFilter i)
                      Base.releaseBloomFilter

-- | Open a database
--
-- The returned handle will automatically be released when the enclosing
-- 'runResourceT' terminates.
open :: MonadResource m => FilePath -> Options -> m DB
open path opts = snd <$> open' path opts

open' :: MonadResource m => FilePath -> Options -> m (ReleaseKey, DB)
open' path opts = allocate (Base.open path opts) Internal.unsafeClose
{-# INLINE open' #-}

-- | Run an action with a snapshot of the database.
--
-- The snapshot will be released when the action terminates or throws an
-- exception. Note that this function is provided for convenience and does not
-- prevent the 'Snapshot' handle to escape. It will, however, be invalid after
-- this function returns and should not be used anymore.
withSnapshot :: MonadResource m => DB -> (Snapshot -> m a) -> m a
withSnapshot db f = do
    (rk, snap) <- createSnapshot' db
    res <- f snap
    release rk
    return res

-- | Create a snapshot of the database.
--
-- The returned 'Snapshot' will be released automatically when the enclosing
-- 'runResourceT' terminates. It is recommended to use 'createSnapshot'' instead
-- and release the resource manually as soon as possible.
createSnapshot :: MonadResource m => DB -> m Snapshot
createSnapshot db = snd <$> createSnapshot' db

-- | Create a snapshot of the database which can (and should) be released early.
createSnapshot' :: MonadResource m => DB -> m (ReleaseKey, Snapshot)
createSnapshot' db = allocate (Base.createSnapshot db) (Base.releaseSnapshot db)

-- | Run an action with an Iterator. The iterator will be closed after the
-- action returns or an error is thrown. Thus, the iterator will /not/ be valid
-- after this function terminates.
withIterator :: MonadResource m => DB -> ReadOptions -> (Iterator -> m a) -> m a
withIterator db opts f = do
    (rk, iter) <- iterOpen' db opts
    res <- f iter
    release rk
    return res

-- | Create an 'Iterator'.
--
-- The iterator will be released when the enclosing 'runResourceT' terminates.
-- You may consider to use 'iterOpen'' instead and manually release the iterator
-- as soon as it is no longer needed (alternatively, use 'withIterator').
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
iterOpen :: MonadResource m => DB -> ReadOptions -> m Iterator
iterOpen db opts = snd <$> iterOpen' db opts

-- | Create an 'Iterator' which can be released early.
iterOpen' :: MonadResource m => DB -> ReadOptions -> m (ReleaseKey, Iterator)
iterOpen' db opts = allocate (Base.createIter db opts) Base.releaseIter
