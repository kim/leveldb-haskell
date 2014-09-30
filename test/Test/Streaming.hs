{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Streaming (tests) where

import           Control.Applicative        hiding (empty)
import           Control.Monad.Catch
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (ByteString, singleton, unpack)
import           Data.Default
import           Data.Foldable              (foldMap)
import           Data.List                  (foldl', intersperse, unfoldr)
import           Data.Monoid
import           Database.LevelDB.Base
import           Database.LevelDB.Internal  (unsafeClose)
import qualified Database.LevelDB.Streaming as S
import           System.Directory
import           System.IO.Temp
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck


type Prop = Test.Tasty.QuickCheck.Property

data Range' = Range' S.Direction Char Char
    deriving Show

asKeyRange :: Range' -> S.KeyRange
asKeyRange (Range' _ s e) =
    let s' = singleton s
        e' = singleton e
      in S.KeyRange s' (`compare` e')

asList :: Range' -> [ByteString]
asList (Range' S.Asc !s !e)
  | s > 'Z'   = []
  | e > 'Z'   = map singleton [s..'Z']
  | otherwise = map singleton [s..e]
asList (Range' S.Desc !s !e)
  | s > 'Z'   = map singleton ['Z'..e]
  | e > 'Z'   = []
  | otherwise = map singleton [s..e]

mkKeySlice :: (Applicative m, MonadIO m) => Range' -> Iterator -> S.Stream m S.Key
mkKeySlice r@(Range' d _ _) i = S.keySlice i (asKeyRange r) d


instance Arbitrary Range' where
    arbitrary = do
        d <- arbitrary
        oneof [ empty d, nonempty d ]
      where
        nonempty d = do
            s <- elements ['A'..'Z']
            e <- case d of
                S.Asc  -> arbitrary `suchThat` (>= s)
                S.Desc -> arbitrary `suchThat` (<= s)
            return $ Range' d s e

        empty d = return $ Range' d '{' '}'

instance Arbitrary S.Direction where
    arbitrary = elements [ S.Asc, S.Desc ]

instance Arbitrary ByteString where
    arbitrary = BS.pack <$> arbitrary

instance CoArbitrary ByteString where
    coarbitrary = coarbitrary . unpack


instance Show (a -> b) where
    show = const "<function>"

data Rs = Rs DB FilePath

tests :: TestTree
tests = withResource initDB destroyDB $ \ rs ->
    testGroup "List-like Iterators"
        [ testGroup "conversions"
            [ testProperty "toList . fromList = id" prop_fromList
            ]
        , testGroup "basic functions"
            [ testProperty "head"        (prop_head        rs)
            , testProperty "append"      (prop_append      rs)
            , testProperty "cons"        (prop_cons        rs)
            , testProperty "snoc"        (prop_snoc        rs)
            , testProperty "last"        (prop_last        rs)
            , testProperty "tail"        (prop_tail        rs)
            , testProperty "init"        (prop_init        rs)
            , testProperty "null"        (prop_null        rs)
            , testProperty "length"      (prop_length      rs)
            ]
        , testGroup "transformations"
            [ testProperty "map"         (prop_map         rs)
            , testProperty "mapM"        (prop_mapM        rs)
            , testProperty "intersperse" (prop_intersperse rs)
            ]
        , testGroup "searching"
            [ testProperty "filter"      (prop_filter      rs)
            ]
        , testGroup "folds"
            [ testProperty "foldl"       (prop_foldl       rs)
            , testProperty "foldl'"      (prop_foldl'      rs)
            , testProperty "foldr"       (prop_foldr       rs)
            , testProperty "foldMap"     (prop_foldMap     rs)
            , testProperty "foldM"       (prop_foldM       rs)
            ]
        , testGroup "special folds"
            [ testProperty "concatMap"   (prop_concatMap   rs)
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
        , testGroup "substreams"
            [ testProperty "take"        (prop_take        rs)
            , testProperty "drop"        (prop_drop        rs)
            , testProperty "takeWhile"   (prop_takeWhile   rs)
            , testProperty "dropWhile"   (prop_dropWhile   rs)
            ]
        , testGroup "zipping and unzipping"
            [ testProperty "zip"         (prop_zip         rs)
            , testProperty "zipWith"     (prop_zipWith     rs)
            , testProperty "unzip"       (prop_unzip       rs)
            ]
        ]
  where
    initDB = do
        tmp <- getTemporaryDirectory
        dir <- createTempDirectory tmp "leveldb-streaming-tests"
        db  <- open dir defaultOptions { createIfMissing = True }
        write db def
            . map ( \ c -> let c' = singleton c in Put c' c')
            $ ['A'..'Z']
        return $ Rs db dir

    destroyDB (Rs db dir) = unsafeClose db `finally` destroy dir defaultOptions


with_iter rs f    = liftIO $ rs >>= \ (Rs db _) -> withIter db def f
run_prop  rs !a b = monadicIO . with_iter rs $! fmap (=== a) . b


--
-- conversions
--

prop_fromList :: [ByteString] -> Prop
prop_fromList xs = monadic runIdentity
                 . fmap (=== xs) . S.toList . S.fromList
                 $ xs

--
-- basic functions
--

prop_append rs range1 range2 = monadicIO $
    with_iter rs $ \ i1 ->
    with_iter rs $ \ i2 ->
        fmap (=== a) $ b i1 i2
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

prop_map :: IO Rs -> Range' -> (ByteString -> Int) -> Prop
prop_map rs range f = run_prop rs a b
  where
    a =              map f $ asList     range
    b = S.toList . S.map f . mkKeySlice range

prop_mapM rs range = monadicIO . with_iter rs $ liftM2 (===) a . b
  where
    a =              mapM f $ asList range
    b = S.toList . S.mapM f . mkKeySlice range

    f = return . BS.length

prop_intersperse rs range x = run_prop rs a b
  where
    a =              intersperse x $ asList     range
    b = S.toList . S.intersperse x . mkKeySlice range

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

prop_foldMap :: IO Rs -> Range' -> (ByteString -> ByteString) -> Prop
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

prop_concatMap rs range = run_prop rs a b
  where
    a =              concatMap (  replicate 10) $ asList     range
    b = S.toList . S.concatMap (S.replicate 10) . mkKeySlice range
--
-- infinite streams
--

prop_iterate :: (Int -> Int) -> Int -> Prop
prop_iterate f x = monadic runIdentity $! fmap (=== a) b
  where
    a =              take 100 $   iterate f x
    b = S.toList . S.take 100 $ S.iterate f x

prop_repeat :: Int -> Prop
prop_repeat x = monadic runIdentity $! fmap (=== a) b
  where
    a =              take 100 $   repeat x
    b = S.toList . S.take 100 $ S.repeat x

prop_replicate :: Int -> Int -> Prop
prop_replicate n x = monadic runIdentity $! fmap (=== a) b
  where
    a =              replicate n x
    b = S.toList $ S.replicate n x

prop_cycle :: NonNegative Int -> Prop
prop_cycle (NonNegative !n) = monadic runIdentity $! fmap (=== a) b
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
prop_unfoldr f z = monadic runIdentity $! fmap (=== a) b
  where
    a =              take 100 $   unfoldr f z
    b = S.toList . S.take 100 $ S.unfoldr f z

--
-- searching
--

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

prop_takeWhile rs range f = run_prop rs a b
  where
    a =              takeWhile f $ asList     range
    b = S.toList . S.takeWhile f . mkKeySlice range

prop_dropWhile rs range f = run_prop rs a b
  where
    a =              dropWhile f $ asList     range
    b = S.toList . S.dropWhile f . mkKeySlice range


--
-- zipping and unzipping
--

prop_zip rs range1 range2 = monadicIO $
    with_iter rs $ \ i1 ->
    with_iter rs $ \ i2 ->
        fmap (=== a) $ b i1 i2
  where
    a       =              zip (asList range1)        (asList range2)
    b i1 i2 = S.toList $ S.zip (mkKeySlice range1 i1) (mkKeySlice range2 i2)

prop_zipWith :: IO Rs
             -> (ByteString -> ByteString -> (ByteString,ByteString))
             -> Range'
             -> Range'
             -> Prop
prop_zipWith rs f range1 range2 = monadicIO $
    with_iter rs $ \ i1 ->
    with_iter rs $ \ i2 ->
        fmap (=== a) $ b i1 i2
  where
    a       =              zipWith f (asList range1)        (asList range2)
    b i1 i2 = S.toList $ S.zipWith f (mkKeySlice range1 i1) (mkKeySlice range2 i2)

prop_unzip rs range1 range2 = monadicIO $
    with_iter rs $ \ i1 ->
    with_iter rs $ \ i2 ->
        fmap (=== a) $ b i1 i2
  where
    a       =   unzip $   zip (asList range1)        (asList range2)
    b i1 i2 = S.unzip $ S.zip (mkKeySlice range1 i1) (mkKeySlice range2 i2)
