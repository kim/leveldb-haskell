{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Database.LevelDB.Streaming
-- Copyright   : (c) 2014 Kim Altintop
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
-- High-level, "Data.List"-like, streaming interface to
-- "Database.LevelDB.Iterator".
--
-- This module contains types and functions to construct 'Stream's from
-- 'Database.LevelDB.Iterator.Iterator's, and re-exports the functions operating
-- on 'Stream's from "Data.Stream.Monadic".
--
-- __Note__ that most of the functions from the latter module are
-- (intentionally) conflicting with the "Prelude", it is thus recommended to
-- import this module qualified:
--
-- > import Database.LevelDB -- or Database.LevelDB.Base
-- > import qualified Database.LevelDB.Streaming as S

module Database.LevelDB.Streaming
    ( Slice     (..)
    , KeyRange  (..)
    , Direction (..)
    , Key
    , Value
    , Entry

    -- * Constructing streams
    , keySlice
    , entrySlice

    -- * Re-exports
    , module Data.Stream.Monadic
    )
where

import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString        (ByteString)
import Data.Stream.Monadic
import Database.LevelDB.Base

import Prelude hiding (drop, filter, foldl, map, mapM, mapM_, take)


data Slice = Slice !Iterator (Maybe KeyRange) !Direction

data KeyRange
    = KeyRange { start :: !ByteString
               , end   :: ByteString -> Ordering
               }
    | AllKeys

data Direction = Asc | Desc
    deriving Show

type Key   = ByteString
type Value = ByteString
type Entry = (Key, Value)


-- | Create a 'Stream' which yields only the keys of the given 'KeyRange' (in
-- the given 'Direction').
--
-- Since traversing the 'Stream' mutates the state of the underlying 'Iterator',
-- it is obviously __unsafe__ to share the latter (between threads, or when
-- 'Data.Stream.Monadic.zip'ping). Hence, it is __highly__ recommended to create
-- a new 'Iterator' for each 'Stream'.
keySlice :: (Applicative m, MonadIO m)
         => Iterator
         -> KeyRange
         -> Direction
         -> Stream m Key
keySlice i (KeyRange s e) d = Stream next (iterSeek i s >> pure i)
  where
    next it = do
        key <- iterKey it
        case key of
            Nothing -> pure Done
            Just k  -> case d of
                Asc  | e k < GT  -> Yield k <$> (iterNext it >> pure it)
                     | otherwise -> pure Done
                Desc | e k > EQ  -> Yield k <$> (iterPrev it >> pure it)
                     | otherwise -> pure Done

keySlice i AllKeys Asc = Stream next (iterFirst i >> pure i)
  where
    next it = iterKey it
          >>= maybe (pure Done) (\ k -> Yield k <$> (iterNext it >> pure it))

keySlice i AllKeys Desc = Stream next (iterLast i >> pure i)
  where
    next it = iterKey it
          >>= maybe (pure Done) (\ k -> Yield k <$> (iterPrev it >> pure it))

-- | Create a 'Stream' which yields key/value pairs of the given 'KeyRange' (in
-- the given 'Direction').
--
-- Since traversing the 'Stream' mutates the state of the underlying 'Iterator',
-- it is obviously __unsafe__ to share the latter (between threads, or when
-- 'Data.Stream.Monadic.zip'ping). Hence, it is __highly__ recommended to create
-- a new 'Iterator' for each 'Stream'.
entrySlice :: (Applicative m, MonadIO m)
           => Iterator
           -> KeyRange
           -> Direction
           -> Stream m Entry
entrySlice i (KeyRange s e) d = Stream next (iterSeek i s >> pure i)
  where
    next it = do
        entry <- iterEntry it
        case entry of
            Nothing       -> pure Done
            Just x@(!k,_) -> case d of
                Asc  | e k < GT  -> Yield x <$> (iterNext it >> pure it)
                     | otherwise -> pure Done
                Desc | e k > EQ  -> Yield x <$> (iterPrev it >> pure it)
                     | otherwise -> pure Done

entrySlice i AllKeys Asc = Stream next (iterFirst i >> pure i)
  where
    next it = iterEntry it
          >>= maybe (pure Done) (\ x -> Yield x <$> (iterNext it >> pure it))

entrySlice i AllKeys Desc = Stream next (iterLast i >> pure i)
  where
    next it = iterEntry it
          >>= maybe (pure Done) (\ x -> Yield x <$> (iterPrev it >> pure it))
