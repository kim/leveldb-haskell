{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Streaming (tests) where

import           Control.Monad.Identity
import           Control.Monad.IO.Class
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (ByteString, singleton, unpack)
import           Data.Default
import           Data.Foldable              (foldMap)
import           Data.List
import           Data.Monoid

import           Prelude

import           Database.LevelDB.Base
import qualified Database.LevelDB.Streaming as S

import           Test.Helpers
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Text.Show.Functions        ()

type Prop = Test.Tasty.QuickCheck.Property

data Range' = Range' S.Direction Char Char
    deriving Show

asKeyRange :: Range' -> S.KeyRange
asKeyRange (Range' _ s e) =
    let s' = singleton s
        e' = singleton e
      in S.KeyRange s' (`compare` e')

asList :: Range' -> [ByteString]
asList (Range' _   '{' '}') = []
asList (Range' S.Asc !s !e)
  | s > 'Z'   = []
  | e > 'Z'   = map singleton [s..'Z']
  | otherwise = map singleton [s..e]
asList (Range' S.Desc !s !e)
  | s > 'Z'   = reverse . map singleton $ [e..'Z']
  | e > 'Z'   = []
  | otherwise = reverse . map singleton $ [e..s]

asAssocList :: Range' -> [(ByteString, ByteString)]
asAssocList r = let r' = asList r in zip r' r'

mkKeySlice :: MonadIO m => Range' -> Iterator -> S.Stream m S.Key
mkKeySlice r@(Range' d _ _) i = S.keySlice i (asKeyRange r) d

mkEntrySlice :: MonadIO m => Range' -> Iterator -> S.Stream m S.Entry
mkEntrySlice r@(Range' d _ _) i = S.entrySlice i (asKeyRange r) d


instance Arbitrary Range' where
    arbitrary = do
        d <- arbitrary
        oneof [ empty d, nonempty d ]
      where
        nonempty d = do
            s <- elements ['A'..'Z']
            e <- case d of
                S.Asc  -> arbitrary `suchThat` (<= 'Z') `suchThat` (>= s)
                S.Desc -> arbitrary `suchThat` (>= 'A') `suchThat` (<= s)
            return $ Range' d s e

        empty d = return $ Range' d '{' '}'

instance Arbitrary S.Direction where
    arbitrary = elements [ S.Asc, S.Desc ]

instance Arbitrary ByteString where
    arbitrary = BS.pack <$> arbitrary

instance CoArbitrary ByteString where
    coarbitrary = coarbitrary . unpack

tests :: TestTree
tests = withResource initDB destroyTestDB $ \db ->
    testGroup "List-like Iterators"
        [ testGroup "conversions"
            [ testProperty "toList . fromList = id" prop_fromList
            ]
        , testGroup "basic functions"
            [ testProperty "head"        (prop_head        db)
            , testProperty "append"      (prop_append      db)
            , testProperty "cons"        (prop_cons        db)
            , testProperty "snoc"        (prop_snoc        db)
            , testProperty "last"        (prop_last        db)
            , testProperty "tail"        (prop_tail        db)
            , testProperty "init"        (prop_init        db)
            , testProperty "null"        (prop_null        db)
            , testProperty "length"      (prop_length      db)
            ]
        , testGroup "transformations"
            [ testProperty "map"         (prop_map         db)
            , testProperty "mapM"        (prop_mapM        db)
            , testProperty "reverse"     (prop_reverse     db)
            , testProperty "intersperse" (prop_intersperse db)
            , testProperty "intercalate" prop_intercalate
            ]
        , testGroup "searching"
            [ testProperty "elem"        (prop_elem        db)
            , testProperty "notElem"     (prop_notElem     db)
            , testProperty "lookup"      (prop_lookup      db)
            , testProperty "find"        (prop_find        db)
            , testProperty "filter"      (prop_filter      db)
            ]
        , testGroup "folds"
            [ testProperty "foldl"       (prop_foldl       db)
            , testProperty "foldl'"      (prop_foldl'      db)
            , testProperty "foldr"       (prop_foldr       db)
            , testProperty "foldMap"     (prop_foldMap     db)
            , testProperty "foldM"       (prop_foldM       db)
            ]
        , testGroup "special folds"
            [ testProperty "concat"      prop_concat
            , testProperty "concatMap"   (prop_concatMap   db)
            , testProperty "and"         prop_and
            , testProperty "or"          prop_or
            , testProperty "any"         prop_any
            , testProperty "all"         prop_all
            , testProperty "sum"         prop_sum
            , testProperty "product"     prop_product
            ]
        , testGroup "scans"
            [ testProperty "scanl"       (prop_scanl       db)
            , testProperty "last (scanl f z xs) == foldl f z xs" (prop_scanl_last db)
            ]
        , testGroup "infinite streams"
            [ testProperty "iterate"     prop_iterate
            , testProperty "repeat"      prop_repeat
            , testProperty "replicate"   prop_replicate
            , testProperty "cycle"       prop_cycle
            ]
        , testGroup "unfolding"
            [ testProperty "unfoldr"     prop_unfoldr
            ]
        , testGroup "predicates"
            [ testProperty "isPrefixOf"  (prop_isPrefixOf  db)
            , testProperty "isSuffixOf"  (prop_isSuffixOf  db)
            ]
        , testGroup "substreams"
            [ testProperty "take"        (prop_take        db)
            , testProperty "drop"        (prop_drop        db)
            , testProperty "splitAt"     (prop_splitAt     db)
            , testProperty "takeWhile"   (prop_takeWhile   db)
            , testProperty "dropWhile"   (prop_dropWhile   db)
            , testProperty "span"        (prop_span        db)
            , testProperty "break"       (prop_break       db)
            ]
        , testGroup "zipping and unzipping"
            [ testProperty "zip"         (prop_zip         db)
            , testProperty "zip3"        (prop_zip3        db)
            , testProperty "zip4"        (prop_zip4        db)
            , testProperty "zipWith"     (prop_zipWith     db)
            , testProperty "zipWith3"    (prop_zipWith3    db)
            , testProperty "zipWith4"    (prop_zipWith4    db)
            , testProperty "unzip"       (prop_unzip       db)
            , testProperty "unzip3"      (prop_unzip3      db)
            , testProperty "unzip4"      (prop_unzip4      db)
            ]
        , testGroup "generalized functions"
            [ testProperty "deleteBy"    (prop_deleteBy    db)
            , testProperty "insertBy"    (prop_insertBy    db)
            ]
        ]
  where
    initDB = do
        db <- initTestDB defaultOptions
        write (testDBHandle db) def
            . map ( \ c -> let c' = singleton c in Put c' c')
            $ ['A'..'Z']
        pure db

with_iter db f    = liftIO $ db >>= \(testDBHandle -> db') -> withIter db' def f
run_prop  db !a b = monadicIO $ with_iter db b >>= assert . (a ==)


--
-- conversions
--

prop_fromList :: [ByteString] -> Prop
prop_fromList xs = monadic runIdentity $
    assert . (xs ==) =<< (S.toList . S.fromList $ xs)

--
-- basic functions
--

prop_append rs range1 range2 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> b i1 i2)
    >>= assert . (a ==)
  where
    a       = asList range1 ++ asList range2
    b i1 i2 = S.toList $ S.append (mkKeySlice range1 i1) (mkKeySlice range2 i2)

prop_cons rs range w = run_prop rs a b
  where
    a = w : asList range
    b = S.toList . S.cons w . mkKeySlice range

prop_snoc rs range y = run_prop rs a b
  where
    a = asList range ++ [y]
    b = S.toList . (`S.snoc` y) . mkKeySlice range

prop_head rs range = run_prop rs a b
  where
    a = case asList range of
            [] -> Nothing
            xs -> Just . head $ xs
    b = S.head . mkKeySlice range

prop_last rs range = run_prop rs a b
  where
    a = case asList range of
            [] -> Nothing
            xs -> Just . last $ xs
    b = S.last . mkKeySlice range

prop_tail rs range = run_prop rs a b
  where
    a = case asList range of
            [] -> []
            xs -> tail xs
    b = S.toList . S.tail . mkKeySlice range

prop_init rs range = run_prop rs a b
  where
    a = case asList range of
            [] -> []
            xs -> init xs
    b = S.toList . S.init . mkKeySlice range

prop_null rs range = run_prop rs a b
  where
    a =   null $ asList     range
    b = S.null . mkKeySlice range

prop_length rs range = run_prop rs a b
  where
    a =   length $ asList     range
    b = S.length . mkKeySlice range


--
-- transformations
--

prop_map :: IO TestDB -> Range' -> (ByteString -> Int) -> Prop
prop_map rs range f = run_prop rs a b
  where
    a =              map f $ asList     range
    b = S.toList . S.map f . mkKeySlice range

prop_mapM rs range = monadicIO . with_iter rs $ liftM2 (===) a . b
  where
    a =              mapM f $ asList range
    b = S.toList . S.mapM f . mkKeySlice range

    f = return . BS.length

prop_reverse rs range = run_prop rs a b
  where
    a =                    reverse $ asList     range
    b = (>>= S.toList) . S.reverse . mkKeySlice range

prop_intersperse rs range x = run_prop rs a b
  where
    a =              intersperse x $ asList     range
    b = S.toList . S.intersperse x . mkKeySlice range

prop_intercalate :: [Int] -> [[Int]] -> Prop
prop_intercalate xs xss = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =              intercalate             xs              xss
    b = S.toList $ S.intercalate (S.fromList xs) (S.fromList xss)

--
-- folds
--

prop_foldl rs range f = run_prop rs a b
  where
    a =   foldl f BS.empty $ asList     range
    b = S.foldl f BS.empty . mkKeySlice range

prop_foldl' rs range f = run_prop rs a b
  where
    a =   foldl' f BS.empty $ asList     range
    b = S.foldl' f BS.empty . mkKeySlice range

prop_foldr rs range f = run_prop rs a b
  where
    a =   foldr f BS.empty $ asList     range
    b = S.foldr f BS.empty . mkKeySlice range

prop_foldMap :: IO TestDB -> Range' -> (ByteString -> ByteString) -> Prop
prop_foldMap rs range f = run_prop rs a b
  where
    a =   foldMap f $ asList     range
    b = S.foldMap f . mkKeySlice range

prop_foldM rs range = monadicIO . with_iter rs $ \ i -> do
    a' <- a
    b' <- b i
    return $! a' === b'
  where
    a =   foldM f BS.empty $ asList     range
    b = S.foldM f BS.empty . mkKeySlice range

    f z x = return $ z <> x

-- TODO: foldM_ ?


--
-- special folds
--

prop_concat :: [[Int]] -> Prop
prop_concat xss = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =              concat                xss
    b = S.toList . S.concat . S.fromList $ xss

prop_concatMap rs range = run_prop rs a b
  where
    a =              concatMap (  replicate 10) $ asList     range
    b = S.toList . S.concatMap (S.replicate 10) . mkKeySlice range

prop_and ts = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =   and                ts
    b = S.and . S.fromList $ ts

prop_or ts = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =   or                ts
    b = S.or . S.fromList $ ts

prop_any :: (Int -> Bool) -> [Int] -> Prop
prop_any p xs = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =   any p                xs
    b = S.any p . S.fromList $ xs

prop_all :: (Int -> Bool) -> [Int] -> Prop
prop_all p xs = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =   all p                xs
    b = S.all p . S.fromList $ xs

prop_sum :: [Int] -> Prop
prop_sum xs = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =   sum                xs
    b = S.sum . S.fromList $ xs

prop_product :: [Int] -> Prop
prop_product xs = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =   product                xs
    b = S.product . S.fromList $ xs

--
-- scans
--

prop_scanl rs f range = run_prop rs a b
  where
    a =              scanl f BS.empty $ asList     range
    b = S.toList . S.scanl f BS.empty . mkKeySlice range

prop_scanl_last rs f range = monadicIO $ do
    (a',b') <- with_iter rs $ \ i -> liftM2 (,) (a i) (b i)
    assert $ a' == Just b'
  where
    a = S.last . S.scanl f BS.empty . mkKeySlice range
    b =          S.foldl f BS.empty . mkKeySlice range

--
-- infinite streams
--

prop_iterate :: (Int -> Int) -> Int -> Prop
prop_iterate f x = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =              take 100 $   iterate f x
    b = S.toList . S.take 100 $ S.iterate f x

prop_repeat :: Int -> Prop
prop_repeat x = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =              take 100 $   repeat x
    b = S.toList . S.take 100 $ S.repeat x

prop_replicate :: Int -> Int -> Prop
prop_replicate n x = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =              replicate n x
    b = S.toList $ S.replicate n x

prop_cycle :: NonNegative Int -> Prop
prop_cycle (NonNegative !n) = monadic runIdentity $! assert . (a ==) =<< b
  where
    a | n == 0    = xs
      | otherwise = take (n*2) . cycle $ xs

    b = S.toList . S.take (n*2) . S.cycle . S.fromList $ xs

    xs :: [Int]
    xs | n == 0    = []
       | otherwise = [0..(n `div` 2)]

--
-- unfolding
--

prop_unfoldr :: (Int -> Maybe (Int, Int)) -> Int -> Prop
prop_unfoldr f z = monadic runIdentity $! assert . (a ==) =<< b
  where
    a =              take 100 $   unfoldr f z
    b = S.toList . S.take 100 $ S.unfoldr f z

--
-- predicates
--

prop_isPrefixOf rs range1 range2 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> b i1 i2)
    >>= assert . (a ==)
  where
    a       =     asList range1    `isPrefixOf`   asList     range2
    b i1 i2 = mkKeySlice range1 i1 `S.isPrefixOf` mkKeySlice range2 i2


prop_isSuffixOf rs range1 range2 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> b i1 i2)
    >>= assert . (a ==)
  where
    a       =     asList range1    `isSuffixOf`   asList     range2
    b i1 i2 = mkKeySlice range1 i1 `S.isSuffixOf` mkKeySlice range2 i2

--
-- searching
--

prop_elem rs range x = run_prop rs a b
  where
    a =  x `elem`      asList     range
    b = (x `S.elem`) . mkKeySlice range

prop_notElem rs range x = run_prop rs a b
  where
    a =  x `notElem`      asList     range
    b = (x `S.notElem`) . mkKeySlice range

prop_lookup rs range k = run_prop rs a b
  where
    a =   lookup k $ asAssocList  range
    b = S.lookup k . mkEntrySlice range

prop_find rs range f = run_prop rs a b
  where
    a =   find f $ asList     range
    b = S.find f . mkKeySlice range

prop_filter rs range f = run_prop rs a b
  where
    a =              filter f $ asList     range
    b = S.toList . S.filter f . mkKeySlice range

--
-- substreams
--

prop_take rs range i = run_prop rs a b
  where
    a =              take i $ asList     range
    b = S.toList . S.take i . mkKeySlice range

prop_drop rs range i = run_prop rs a b
  where
    a =              drop i $ asList     range
    b = S.toList . S.drop i . mkKeySlice range

prop_splitAt rs range i = run_prop rs a b
  where
    a =             splitAt i $ asList     range
    b = toLists . S.splitAt i . mkKeySlice range

prop_takeWhile rs range f = run_prop rs a b
  where
    a =              takeWhile f $ asList     range
    b = S.toList . S.takeWhile f . mkKeySlice range

prop_dropWhile rs range f = run_prop rs a b
  where
    a =              dropWhile f $ asList     range
    b = S.toList . S.dropWhile f . mkKeySlice range

prop_span rs range p = run_prop rs a b
  where
    a =             span p $ asList     range
    b = toLists . S.span p . mkKeySlice range

prop_break rs range p = run_prop rs a b
  where
    a =             break p $ asList     range
    b = toLists . S.break p . mkKeySlice range

--
-- zipping and unzipping
--

prop_zip rs range1 range2 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> b i1 i2)
    >>= assert . (a ==)
  where
    a       =              zip (asList range1)        (asList range2)
    b i1 i2 = S.toList $ S.zip (mkKeySlice range1 i1) (mkKeySlice range2 i2)

prop_zip3 rs range1 range2 range3 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> with_iter rs $ \ i3 -> b i1 i2 i3)
    >>= assert . (a ==)
  where
    a          =              zip3 (asList range1)        (asList range2)        (asList range3)
    b i1 i2 i3 = S.toList $ S.zip3 (mkKeySlice range1 i1) (mkKeySlice range2 i2) (mkKeySlice range3 i3)

prop_zip4 rs range1 range2 range3 range4 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> with_iter rs $ \ i3 -> with_iter rs $ \ i4 -> b i1 i2 i3 i4)
    >>= assert . (a ==)
  where
    a             =              zip4 (asList range1)        (asList range2)        (asList range3)        (asList range4)
    b i1 i2 i3 i4 = S.toList $ S.zip4 (mkKeySlice range1 i1) (mkKeySlice range2 i2) (mkKeySlice range3 i3) (mkKeySlice range4 i4)

prop_zipWith :: IO TestDB
             -> (ByteString -> ByteString -> (ByteString,ByteString))
             -> Range'
             -> Range'
             -> Prop
prop_zipWith rs f range1 range2 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> b i1 i2)
    >>= assert . (a ==)
  where
    a       =              zipWith f (asList range1)        (asList range2)
    b i1 i2 = S.toList $ S.zipWith f (mkKeySlice range1 i1) (mkKeySlice range2 i2)

prop_zipWith3 :: IO TestDB
              -> (ByteString -> ByteString -> ByteString -> (ByteString, ByteString,ByteString))
              -> Range'
              -> Range'
              -> Range'
              -> Prop
prop_zipWith3 rs f range1 range2 range3 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> with_iter rs $ \ i3 -> b i1 i2 i3)
    >>= assert . (a ==)
  where
    a          =              zipWith3 f (asList range1)        (asList range2)        (asList range3)
    b i1 i2 i3 = S.toList $ S.zipWith3 f (mkKeySlice range1 i1) (mkKeySlice range2 i2) (mkKeySlice range3 i3)

prop_zipWith4 :: IO TestDB
              -> (ByteString -> ByteString -> ByteString -> ByteString -> (ByteString, ByteString, ByteString,ByteString))
              -> Range'
              -> Range'
              -> Range'
              -> Range'
              -> Prop
prop_zipWith4 rs f range1 range2 range3 range4 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> with_iter rs $ \ i3 -> with_iter rs $ \ i4 -> b i1 i2 i3 i4)
    >>= assert . (a ==)
  where
    a             =              zipWith4 f (asList range1)        (asList range2)        (asList range3)        (asList range4)
    b i1 i2 i3 i4 = S.toList $ S.zipWith4 f (mkKeySlice range1 i1) (mkKeySlice range2 i2) (mkKeySlice range3 i3) (mkKeySlice range4 i4)

prop_unzip rs range1 range2 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> b i1 i2)
    >>= assert . (a ==)
  where
    a       =   unzip $   zip (asList range1)        (asList range2)
    b i1 i2 = S.unzip $ S.zip (mkKeySlice range1 i1) (mkKeySlice range2 i2)

prop_unzip3 rs range1 range2 range3 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> with_iter rs $ \ i3 -> b i1 i2 i3)
    >>= assert . (a ==)
  where
    a          =   unzip3 $   zip3 (asList range1)        (asList range2)        (asList range3)
    b i1 i2 i3 = S.unzip3 $ S.zip3 (mkKeySlice range1 i1) (mkKeySlice range2 i2) (mkKeySlice range3 i3)

prop_unzip4 rs range1 range2 range3 range4 = monadicIO $
        (with_iter rs $ \ i1 -> with_iter rs $ \ i2 -> with_iter rs $ \ i3 -> with_iter rs $ \ i4 -> b i1 i2 i3 i4)
    >>= assert . (a ==)
  where
    a             =   unzip4 $   zip4 (asList range1)        (asList range2)        (asList range3)        (asList range4)
    b i1 i2 i3 i4 = S.unzip4 $ S.zip4 (mkKeySlice range1 i1) (mkKeySlice range2 i2) (mkKeySlice range3 i3) (mkKeySlice range4 i4)

--
-- generalized
--

prop_deleteBy rs eq x range = run_prop rs a b
  where
    a =              deleteBy eq x $ asList     range
    b = S.toList . S.deleteBy eq x . mkKeySlice range

prop_insertBy rs cmp x range = run_prop rs a b
  where
    a =              insertBy cmp x $ asList     range
    b = S.toList . S.insertBy cmp x . mkKeySlice range

--
-- Helpers
--

toLists :: Monad m => (S.Stream m a, S.Stream m a) -> m ([a], [a])
toLists (s1,s2) = liftM2 (,) (S.toList s1) (S.toList s2)
