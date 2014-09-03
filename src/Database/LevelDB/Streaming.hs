{-# LANGUAGE BangPatterns #-}

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
