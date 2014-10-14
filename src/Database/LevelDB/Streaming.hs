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
    ( KeyRange  (..)
    , Direction (..)
    , Key
    , Value
    , Entry

    -- * Constructing streams
    -- $caveats
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


-- $caveats
-- Caveats:
--
-- * remember that 'Iterator's are /not/ threadsafe
-- * consider that traversing a 'Stream' mutates the underlying 'Iterator', so
-- the above applies to the 'Stream' as well
-- * because of the destructive update semantics of 'Iterator's, the following
-- example will (perhaps obviously) /not/ work as expected:
--
-- > withIter db def $ \ i ->
-- >     toList $ zip (keySlice i AllKeys Asc) (keySlice i AllKeys Desc)
--
-- However, the following /will/ work:
--
-- > withIter db def $ \ i ->
-- >     foldl (+) 0 . map (*2) . map ByteString.length $ keySlice i AllKeys Asc
--
-- Here, fusion ensures the key slice is traversed only once, while the next
-- example will incur rewinding the 'Iterator' and traversing it a second time:
--
-- > withIter db def $ \ i ->
-- >     let slice   = keySlice i AllKeys Asc
-- >         count f = foldl' (\ c k -> c + f k) 0
-- >      in liftM2 (+) (count ByteString.length slice) (count (const 1) slice)
--
-- To summarise: it is recommended to always create 'Stream's with their own
-- exclusive 'Iterator', and to not share them across threads.

-- | Create a 'Stream' which yields only the keys of the given 'KeyRange' (in
-- the given 'Direction').
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
                Desc | e k > LT  -> Yield k <$> (iterPrev it >> pure it)
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
                Desc | e k > LT  -> Yield x <$> (iterPrev it >> pure it)
                     | otherwise -> pure Done

entrySlice i AllKeys Asc = Stream next (iterFirst i >> pure i)
  where
    next it = iterEntry it
          >>= maybe (pure Done) (\ x -> Yield x <$> (iterNext it >> pure it))

entrySlice i AllKeys Desc = Stream next (iterLast i >> pure i)
  where
    next it = iterEntry it
          >>= maybe (pure Done) (\ x -> Yield x <$> (iterPrev it >> pure it))
