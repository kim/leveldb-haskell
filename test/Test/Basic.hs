{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Test.Basic
where

import           Control.Applicative        (liftA2)
import           Data.Default               (def)
import           Data.Foldable              (length, toList)
import           Data.List                  ((\\))

import qualified Database.LevelDB.Base      as L
import qualified Database.LevelDB.Streaming as S

import           Test.Helpers

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

tests :: IO Bool
tests = checkParallel $$(discover)

prop_putGet :: Property
prop_putGet = property $ do
    (k,v) <- forAll $ liftA2 (,) genKey genVal
    v'    <-
        evalIO .  withTestDB $ \(testDBHandle -> db) -> do
            L.put db def k v
            L.get db def k
    v' === Just v

prop_delete :: Property
prop_delete = property $ do
    (k,v) <- forAll $ liftA2 (,) genKey genVal
    v'    <-
        evalIO . withTestDB $ \(testDBHandle -> db) -> do
            L.put db def k v
            L.delete db def k
            L.get db def k
    v' === Nothing

prop_putBatch :: Property
prop_putBatch = property $ do
    (ks, vs) <- forAll genEntries
    kvs      <-
        evalIO .  withTestDB $ \(testDBHandle -> db) -> do
            L.write    db def $ zipWith L.Put ks vs
            L.withIter db def $ \it ->
                S.toList (S.entrySlice it S.AllKeys S.Asc)
    kvs === zip ks vs

prop_deleteBatch :: Property
prop_deleteBatch = property $ do
    (ks, vs) <- forAll genEntries
    del      <- forAll $ Gen.list (Range.constant 1 (length ks)) (Gen.element ks)
    ks'      <-
        evalIO .  withTestDB $ \(testDBHandle -> db) -> do
            L.write    db def $ zipWith L.Put ks vs
            L.write    db def $ map L.Del del
            L.withIter db def $ \it ->
                S.toList (S.keySlice it S.AllKeys S.Asc)
    ks' === ks \\ del

-- We can delete keys written in the same batch, but the order of 'BatchOps'
-- matters!
prop_deleteBatchSingleWrite :: Property
prop_deleteBatchSingleWrite = property $ do
    (ks, vs) <- forAll genEntries
    del      <- forAll $ Gen.list (Range.constant 1 (length ks)) (Gen.element ks)
    ks'      <-
        evalIO .  withTestDB $ \(testDBHandle -> db) -> do
            L.write    db def $ zipWith L.Put ks vs <> map L.Del del
            L.withIter db def $ \it ->
                S.toList (S.keySlice it S.AllKeys S.Asc)
    ks' === ks \\ del

prop_snapshots :: Property
prop_snapshots = property $ do
    (ks, vs) <- forAll genEntries
    (as, bs) <-
        evalIO .  withTestDB $ \(testDBHandle -> db) ->
            L.withSnapshot db $ \snap -> do
                L.write db def $ zipWith L.Put ks vs
                as <-
                    L.withIter db def $ \it ->
                        S.toList (S.keySlice it S.AllKeys S.Asc)
                bs <-
                    L.withIter db def { L.useSnapshot = Just snap } $ \it ->
                        S.toList (S.keySlice it S.AllKeys S.Asc)
                pure (as, bs)
    as /== bs
    as === ks
    bs === []

genEntries = do
    ks <- toList <$> Gen.set (Range.constantFrom 3 10 30) genKey
    vs <- toList <$> Gen.nonEmpty (Range.singleton (length ks)) genVal
    pure (ks, vs)

genKey = Gen.bytes (Range.singleton 8)
genVal = Gen.bytes (Range.singleton 32)
