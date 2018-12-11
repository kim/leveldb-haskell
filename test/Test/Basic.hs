{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Test.Basic
where

import           Control.Applicative        (liftA2)
import           Data.ByteString            (ByteString)
import           Data.Default               (def)
import           Data.Foldable              (length, toList)
import qualified Data.HashSet               as Set
import           Data.Ord                   (Down (..), comparing)

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
    opts  <- forAllWith showOptions genOptions
    (k,v) <- forAll $ liftA2 (,) genKey genVal
    v'    <-
        evalIO . withTestDB opts $ \(testDBHandle -> db) -> do
            L.put db def k v
            L.get db def k
    v' === Just v

prop_delete :: Property
prop_delete = property $ do
    opts  <- forAllWith showOptions genOptions
    (k,v) <- forAll $ liftA2 (,) genKey genVal
    v'    <-
        evalIO . withTestDB opts $ \(testDBHandle -> db) -> do
            L.put db def k v
            L.delete db def k
            L.get db def k
    v' === Nothing

prop_putBatch :: Property
prop_putBatch = property $ do
    opts  <- forAllWith showOptions genOptions
    (ks, vs) <- forAll genEntries
    kvs      <-
        evalIO . withTestDB opts $ \(testDBHandle -> db) -> do
            L.write    db def $ zipWith L.Put ks vs
            L.withIter db def $ \it ->
                S.toList (S.entrySlice it S.AllKeys S.Asc)

    Set.fromList kvs === Set.fromList (zip ks vs)

prop_deleteBatch :: Property
prop_deleteBatch = property $ do
    opts     <- forAllWith showOptions genOptions
    (ks, vs) <- forAll genEntries
    del      <- forAll $ Gen.list (Range.constant 1 (length ks)) (Gen.element ks)
    ks'      <-
        evalIO . withTestDB opts $ \(testDBHandle -> db) -> do
            L.write    db def $ zipWith L.Put ks vs
            L.write    db def $ map L.Del del
            L.withIter db def $ \it ->
                S.toList (S.keySlice it S.AllKeys S.Asc)

    Set.fromList ks' === Set.difference (Set.fromList ks) (Set.fromList del)

-- We can delete keys written in the same batch, but the order of 'BatchOps'
-- matters!
prop_deleteBatchSingleWrite :: Property
prop_deleteBatchSingleWrite = property $ do
    opts     <- forAllWith showOptions genOptions
    (ks, vs) <- forAll genEntries
    del      <- forAll $ Gen.list (Range.constant 1 (length ks)) (Gen.element ks)
    ks'      <-
        evalIO . withTestDB opts $ \(testDBHandle -> db) -> do
            L.write    db def $ zipWith L.Put ks vs <> map L.Del del
            L.withIter db def $ \it ->
                S.toList (S.keySlice it S.AllKeys S.Asc)

    Set.fromList ks' === Set.difference (Set.fromList ks) (Set.fromList del)

prop_snapshots :: Property
prop_snapshots = property $ do
    opts     <- forAllWith showOptions genOptions
    (ks, vs) <- forAll genEntries
    (as, bs) <-
        evalIO . withTestDB opts $ \(testDBHandle -> db) ->
            L.withSnapshot db $ \snap -> do
                L.write db def $ zipWith L.Put ks vs
                as <-
                    L.withIter db def $ \it ->
                        S.toList (S.keySlice it S.AllKeys S.Asc)
                bs <-
                    L.withIter db def { L.useSnapshot = Just snap } $ \it ->
                        S.toList (S.keySlice it S.AllKeys S.Asc)
                pure (as, bs)

    let
        as' = Set.fromList as
        bs' = Set.fromList bs
        ks' = Set.fromList ks
     in do
        as' /== bs'
        as' === ks'
        bs === []

-- Generators ------------------------------------------------------------------

genEntries :: Gen ([ByteString], [ByteString])
genEntries = do
    ks <- toList <$> Gen.set (Range.constantFrom 3 10 30) genKey
    vs <- toList <$> Gen.nonEmpty (Range.singleton (length ks)) genVal
    pure (ks, vs)

genKey, genVal :: Gen ByteString
genKey = Gen.bytes (Range.singleton 8)
genVal = Gen.bytes (Range.singleton 32)

genOptions :: Gen L.Options
genOptions = do
    cacheSize    <- Gen.int (Range.constant 0 32000000)
    comparator   <- Gen.maybe (pure $ L.Comparator (comparing Down))
    compression  <- Gen.element [L.NoCompression, L.Snappy]
    --filterPolicy <- Gen.maybe (Left <$> L.createBloomFilter 10)
    pure L.defaultOptions
        { L.cacheSize    = cacheSize
        , L.comparator   = comparator
        , L.compression  = compression
     --   , L.filterPolicy = filterPolicy
        }

showOptions :: L.Options -> String
showOptions opts = unlines
    [ "defaultOptions"
    , "    { cacheSize    = ", show $ L.cacheSize opts
    , "    , comparator   = ", maybe "Nothing" (const $ "Just <fn>") $ L.comparator opts
    , "    , compression  = ", show $ L.compression opts
    , "    , filterPolicy = ", maybe "Nothing" (const $ "Just <bloom>") $ L.filterPolicy opts
    , "    }"
    ]
