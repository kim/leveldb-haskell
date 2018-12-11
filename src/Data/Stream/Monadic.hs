{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Data.Stream.Monadic
-- Copyright   : (c) 2014 Kim Altintop
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
-- (Mostly mechanical) adaptation of the
-- <http://hackage.haskell.org/package/stream-fusion/docs/Data-Stream.html Data.Stream>
-- module from the
-- <http://hackage.haskell.org/package/stream-fusion stream-fusion> package to a
-- monadic 'Stream' datatype similar to the one
-- <https://www.fpcomplete.com/blog/2014/08/conduit-stream-fusion proposed> by
-- Michael Snoyman for the <http://hackage.haskell.org/package/conduit conduit>
-- package.
--
-- The intention here is to provide a high-level, "Data.List"-like interface to
-- "Database.LevelDB.Iterator"s with predictable space and time complexity (see
-- "Database.LevelDB.Streaming"), and without introducing a dependency eg. on
-- one of the streaming libraries (all relevant datatypes are fully exported,
-- though, so it should be straightforward to write wrappers for your favourite
-- streaming library).
--
-- Fusion and inlining rules and strictness annotations have been put in place
-- faithfully, and may need further profiling. Also, some functions (from
-- "Data.List") have been omitted for various reasons. Missing functions may be
-- added upon <https://github.com/kim/leveldb-haskell/pulls request>.

module Data.Stream.Monadic
    ( Step   (..)
    , Stream (..)

    -- * Conversion with lists
    , toList
    , fromList

    -- * Basic functions
    , append
    , cons
    , snoc
    , head
    , last
    , tail
    , init
    , null
    , length -- finitary

    -- * Transformations
    , map
    , mapM
    , mapM_
    , reverse
    , intersperse
    , intercalate

    -- * Folds
    , foldl
    , foldl'
    -- , foldl1
    -- , foldl1'
    , foldr
    -- , foldr1
    , foldMap
    , foldM
    , foldM_

    -- * Special folds
    , concat
    , concatMap
    , and
    , or
    , any
    , all
    , sum
    , product
    --, maximum -- non-empty
    --, minimum -- non-empty

    -- * Building streams
    -- ** Scans
    , scanl
    -- , scanl1
    -- , scanr
    -- , scanr1

    -- Accumulating maps
    -- , mapAccumL
    -- , mapAccumR

    -- ** Infinite streams
    , iterate
    , repeat
    , replicate
    , cycle

    -- ** Unfolding
    , unfoldr
    , unfoldrM

    -- * Substreams
    -- ** Extracting substreams
    , take
    , drop
    , splitAt
    , takeWhile
    , dropWhile
    , span
    , break
    -- , group
    -- , inits
    -- , tails

    -- ** Predicates
    , isPrefixOf
    , isSuffixOf
    -- , isInfixOf -- would need 'tails'

    -- * Searching streams
    -- ** Searching by equality
    , elem
    , notElem
    , lookup

    -- ** Searching with a predicate
    , find
    , filter
    -- , partition

    -- Indexing streams
    --   does not make too much sense
    -- , index
    -- , findIndex
    -- , elemIndex
    -- , elemIndices
    -- , findIndices

    -- * Zipping and unzipping
    , zip
    , zip3
    , zip4
    , zipWith
    , zipWith3
    , zipWith4
    , unzip
    , unzip3
    , unzip4

    -- * Special streams
    --   strings - not applicable
    -- , lines
    -- , words
    -- , unlines
    -- , unwords

    -- ** \"Set\" operations
    -- , nub
    , delete
    -- , \\
    -- , union
    -- , intersect

    -- , sort
    , insert

    -- * Generalized functions

    --   User-supplied equality, replacing an Eq context
    -- , nubBy
    , deleteBy
    -- , deleteFirstsBy
    -- , unionBy
    -- , intersectBy
    -- , groupBy

    -- ** User-supplied comparison, replacing an Ord context
    -- , sortBy
    , insertBy
    -- , maximumBy
    -- , minimumBy

    -- * The \"generic\" operations
    , genericLength
    , genericTake
    , genericDrop
    , genericSplitAt
    -- , genericIndex
    , genericReplicate

    , enumFromToInt
    , enumFromToChar
    , enumDeltaInteger
    )
where

import Control.Applicative
import Control.Monad       (Monad (..), void, (=<<), (>=>))
import Data.Char           (Char, chr, ord)
import Data.Monoid

import Prelude (Bool (..), Either (..), Eq (..), Functor (..), Int, Integer,
                Integral (..), Maybe (..), Num (..), Ord (..), Ordering (..),
                error, flip, not, otherwise, undefined, ($), (&&), (.), (||))


data Step   a  s
   = Yield  a !s
   | Skip  !s
   | Done

data Stream m a = forall s. Stream (s -> m (Step a s)) (m s)

instance Monad m => Functor (Stream m) where
    fmap = map


toList :: (Monad m) => Stream m a -> m [a]
toList (Stream next s0) = unfold =<< s0
  where
    unfold !s = do
        step <- next s
        case step of
            Done       -> return []
            Skip    s' -> unfold s'
            Yield x s' -> (x :) <$> unfold s'
{-# INLINE [0] toList #-}

fromList :: Monad m => [a] -> Stream m a
fromList xs = Stream next (return xs)
  where
    {-# INLINE next #-}
    next []      = return Done
    next (x:xs') = return $ Yield x xs'
{-# INLINE [0] fromList #-}
{-# RULES
"Stream fromList/toList fusion" forall s.
    fmap fromList (toList s) = return s
  #-}

append :: (Monad m) => Stream m a -> Stream m a -> Stream m a
append (Stream next0 s0) (Stream next1 s1) = Stream next (Left <$> s0)
  where
    {-# INLINE next #-}
    next (Left s) = do
        step <- next0 s
        case step of
            Done       -> Skip . Right <$> s1
            Skip    s' -> return $ Skip    (Left s')
            Yield x s' -> return $ Yield x (Left s')

    next (Right s) = do
        step <- next1 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip    (Right s')
            Yield x s' -> Yield x (Right s')
{-# INLINE [0] append #-}

cons :: (Monad m) => a -> Stream m a -> Stream m a
cons w (Stream next0 s0) = Stream next ((,) S2 <$> s0)
  where
    {-# INLINE next #-}
    next (S2, s) = return $ Yield w (S1, s)
    next (S1, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip    (S1, s')
            Yield x s' -> Yield x (S1, s')
{-# INLINE [0] cons #-}

snoc :: (Monad m) => Stream m a -> a -> Stream m a
snoc (Stream next0 s0) y = Stream next (Just <$> s0)
  where
    {-# INLINE next #-}
    next Nothing  = return Done
    next (Just s) = do
        step <- next0 s
        return $ case step of
            Done       -> Yield y Nothing
            Skip    s' -> Skip    (Just s')
            Yield x s' -> Yield x (Just s')
{-# INLINE [0] snoc #-}

-- | Unlike 'Data.List.head', this function does not diverge if the 'Stream' is
-- empty. Instead, 'Nothing' is returned.
head :: Monad m => Stream m a -> m (Maybe a)
head (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Yield x _  -> return $ Just x
            Skip    s' -> loop s'
            Done       -> return Nothing
{-# INLINE [0] head #-}

-- | Unlike 'Data.List.last', this function does not diverge if the 'Stream' is
-- empty. Instead, 'Nothing' is returned.
last :: Monad m => Stream m a -> m (Maybe a)
last (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done       -> return Nothing
            Skip    s' -> loop s'
            Yield x s' -> loop' x s'
    loop' x !s = do
        step <- next s
        case step of
            Done        -> return $ Just x
            Skip     s' -> loop' x s'
            Yield x' s' -> loop' x' s'
{-# INLINE [0] last #-}

data Switch = S1 | S2

-- | Unlike 'Data.List.tail', this function does not diverge if the 'Stream' is
-- empty. Instead, it is the identity in this case.
tail :: (Monad m) => Stream m a -> Stream m a
tail (Stream next0 s0) = Stream next ((,) S1 <$> s0)
  where
    {-# INLINE next #-}
    next (S1, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip (S1, s')
            Yield _ s' -> Skip (S2, s')

    next (S2, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip (S2, s')
            Yield x s' -> Yield x (S2, s')
{-# INLINE [0] tail #-}

-- | Unlike 'Data.List.init', this function does not diverge if the 'Stream' is
-- empty. Instead, it is the identity in this case.
init :: (Monad m) => Stream m a -> Stream m a
init (Stream next0 s0) = Stream next ((,) Nothing <$> s0)
  where
    {-# INLINE next #-}
    next (Nothing, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip (Nothing, s')
            Yield x s' -> Skip (Just x , s')

    next (Just x, s) = do
        step <- next0 s
        return $ case step of
            Done        -> Done
            Skip     s' -> Skip    (Just x , s')
            Yield x' s' -> Yield x (Just x', s')
{-# INLINE [0] init #-}

null :: Monad m => Stream m a -> m Bool
null (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done       -> return True
            Yield _ _  -> return False
            Skip    s' -> loop s'
{-# INLINE [0] null #-}

length :: Monad m => Stream m a -> m Int
length (Stream next s0) = loop 0 =<< s0
  where
    loop !z !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop   z    s'
            Yield _ s' -> loop  (z+1) s'
{-# INLINE [0] length #-}

elem :: (Eq a, Monad m) => a -> Stream m a -> m Bool
elem x (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done                   -> return False
            Skip    s'             -> loop s'
            Yield y s' | y == x    -> return True
                       | otherwise -> loop s'
{-# INLINE [0] elem #-}

notElem :: (Eq a, Monad m) => a -> Stream m a -> m Bool
notElem x s = elem x s >>= return . not

lookup :: (Eq a, Monad m) => a -> Stream m (a, b) -> m (Maybe b)
lookup key (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done                        -> return Nothing
            Skip s'                     -> loop s'
            Yield (x, y) s' | key == x  -> return $ Just y
                            | otherwise -> loop s'
{-# INLINE [0] lookup #-}

find :: Monad m => (a -> Bool) -> Stream m a -> m (Maybe a)
find p = head . filter p
{-# INLINE [0] find #-}

filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter p (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next !s = do
        step <- next0 s
        return $ case step of
            Done                   -> Done
            Skip    s'             -> Skip    s'
            Yield x s' | p x       -> Yield x s'
                       | otherwise -> Skip    s'
{-# INLINE [0] filter #-}
{-# RULES
"Stream filter/filter fusion" forall p q s.
    filter p (filter q s) = filter (\ x -> q x && p x) s
  #-}

map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next !s = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip        s'
            Yield x s' -> Yield (f x) s'
{-# INLINE [0] map #-}
{-# RULES
"Stream map/map fusion" forall f g s.
    map f (map g s) = map (f . g) s
  #-}

mapM :: (Monad m) => (a -> m b) -> Stream m a -> Stream m b
mapM f (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next !s = do
        step <- next0 s
        case step of
            Done       -> return Done
            Skip    s' -> return $ Skip s'
            Yield x s' -> (`Yield` s') <$> f x
{-# INLINE [0] mapM #-}
{-# RULES
"Stream mapM/mapM fusion" forall f g s.
    mapM f (mapM g s) = mapM (g >=> f) s

"Stream map/mapM fusion" forall f g s.
    map f (mapM g s)  = mapM (fmap f . g) s

"Stream mapM/map fusion" forall f g s.
    mapM f (map g s)  = mapM (f . g) s
  #-}

mapM_ :: (Monad m) => (a -> m b) -> Stream m a -> Stream m ()
mapM_ f s = Stream go (return ())
  where
    {-# INLINE go #-}
    go _ = foldM_ (\ _ -> void . f) () s >> return Done
{-# INLINE [0] mapM_ #-}
{-# RULES
"Stream mapM_/mapM fusion" forall f g s.
    mapM_ f (mapM g s) = mapM_ (g >=> f) s

"Stream mapM_/map fusion" forall f g s.
    mapM_ f (map g s)  = mapM_ (f . g) s
  #-}

reverse :: (Monad m) => Stream m a -> m (Stream m a)
reverse = foldl' (flip cons) (fromList [])
{-# INLINE reverse #-}

intersperse :: (Monad m) => a -> Stream m a -> Stream m a
intersperse sep (Stream next0 s0) = Stream next ((,,) Nothing S1 <$> s0)
  where
    {-# INLINE next #-}
    next (Nothing, S1, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip (Nothing, S1, s')
            Yield x s' -> Skip (Just x , S1, s')

    next (Just x, S1, s)  = return $ Yield x (Nothing, S2, s)

    next (Nothing, S2, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip      (Nothing, S2, s')
            Yield x s' -> Yield sep (Just x , S1, s')

    next (Just _, S2, _)  = error "Data.Stream.Monadic.intersperse: impossible"
{-# INLINE [0] intersperse #-}

intercalate :: (Monad m) => Stream m a -> Stream m [a] -> Stream m a
intercalate sep s = first s `append` rest s
  where
    first = concat                            . take 1
    rest  = concatMap (append sep . fromList) . drop 1
{-# INLINE intercalate #-}

--transpose :: Monad m => Stream m [a] -> Stream m [a]

foldMap :: (Monoid w, Monad m) => (a -> w) -> Stream m a -> m w
foldMap f (Stream next s0) = loop mempty =<< s0
  where
    loop z !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop z s'
            Yield x s' -> loop (z <> f x) s'
{-# INLINE [0] foldMap #-}
{-# RULES
"Stream foldMap/map fusion" forall f g s.
    foldMap f (map g s)  = foldMap (f . g) s

"Stream foldMap/mapM fusion" forall f g s.
    foldMap f (mapM g s) = foldM (\ z' -> fmap ((z' <>) . f) . g) mempty s
  #-}

foldl :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl f z0 (Stream next s0) = loop z0 =<< s0
  where
    loop z !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop z s'
            Yield x s' -> loop (f z x) s'
{-# INLINE [0] foldl #-}
{-# RULES
"Stream foldl/map fusion" forall f g z s.
    foldl f z (map g s)  = foldl (\ z' -> f z' . g) z s

"Stream foldl/mapM fusion" forall f g z s.
    foldl f z (mapM g s) = foldM (\ z' -> fmap (f z') . g) z s
  #-}

foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl' f z0 (Stream next s0) = loop z0 =<< s0
  where
    loop !z !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop z s'
            Yield x s' -> loop (f z x) s'
{-# INLINE [0] foldl' #-}
{-# RULES
"Stream foldl'/map fusion" forall f g z s.
    foldl' f z (map g s)  = foldl' (\ z' -> f z' . g) z s

"Stream foldl'/mapM fusion" forall f g z s.
    foldl' f z (mapM g s) = foldM  (\ z' -> fmap (f z') . g) z s
  #-}

foldr :: (Monad m) => (a -> b -> b) -> b -> Stream m a -> m b
foldr f z (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop s'
            Yield x s' -> f x <$> loop s'
{-# INLINE [0] foldr #-}
{-# RULES
"Stream foldr/map fusion" forall f g z s.
    foldr f z (map g s)  = foldr (f . g) z s

"Stream foldr/mapM fusion" forall f g z s.
    foldr f z (mapM g s) = foldM (\ z' -> fmap (`f` z') . g) z s
  #-}

foldM :: Monad m => (b -> a -> m b) -> b -> Stream m a -> m b
foldM f z0 (Stream next s0) = loop z0 =<< s0
  where
    loop z !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop z s'
            Yield x s' -> f z x >>= (`loop` s')
{-# INLINE [0] foldM #-}
{-# RULES
"Stream foldM/map fusion" forall f g z s.
    foldM f z (map g s)  = foldM (\ z' -> f z' . g) z s

"Stream foldM/mapM fusion" forall f g z s.
    foldM f z (mapM g s) = foldM (\ z' -> g >=> f z') z s
  #-}

foldM_ :: Monad m => (b -> a -> m b) -> b -> Stream m a -> m ()
foldM_ f z s = foldM f z s >> return ()
{-# INLINE foldM_ #-}

concat :: (Monad m) => Stream m [a] -> Stream m a
concat = concatMap fromList
{-# INLINE concat #-}

concatMap :: (Monad m) => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap f (Stream next0 s0) = Stream next ((,) Nothing <$> s0)
  where
    {-# INLINE next #-}
    next (Nothing, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip (Nothing   , s')
            Yield x s' -> Skip (Just (f x), s')

    next (Just (Stream g t), s) = do
        step <- g =<< t
        return $ case step of
            Done       -> Skip    (Nothing                    , s)
            Skip    t' -> Skip    (Just (Stream g (return t')), s)
            Yield x t' -> Yield x (Just (Stream g (return t')), s)
{-# INLINE [0] concatMap #-}
{-# RULES
"Stream concatMap/map fusion" forall f g s.
    concatMap f (map g s) = concatMap (f . g) s
  #-}

and :: (Monad m) => Stream m Bool -> m Bool
and = foldr (&&) True
{-# INLINE and #-}

or :: (Monad m) => Stream m Bool -> m Bool
or = foldr (||) False
{-# INLINE or #-}

any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
any p (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done                   -> return False
            Skip    s'             -> loop s'
            Yield x s' | p x       -> return True
                       | otherwise -> loop s'
{-# INLINE [0] any #-}

all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
all p (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done                   -> return True
            Skip    s'             -> loop s'
            Yield x s' | p x       -> loop s'
                       | otherwise -> return False
{-# INLINE [0] all #-}

sum :: (Num a, Monad m) => Stream m a -> m a
sum (Stream next s0) = loop 0 =<< s0
  where
    loop !a !s = do
        step <- next s
        case step of
            Done       -> return a
            Skip    s' -> loop   a      s'
            Yield x s' -> loop  (a + x) s'
{-# INLINE [0] sum #-}

product :: (Num a, Monad m) => Stream m a -> m a
product (Stream next s0) = loop 1 =<< s0
  where
    loop !a !s = do
        step <- next s
        case step of
            Done       -> return a
            Skip    s' -> loop   a      s'
            Yield x s' -> loop  (a * x) s'
{-# INLINE [0] product #-}

scanl :: (Monad m) => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl f z0 = go . (`snoc` undefined)
  where
    {-# INLINE go #-}
    go (Stream step s0) = Stream (next step) ((,) z0 <$> s0)

    {-# INLINE next #-}
    next step (z, s) = do
        step' <- step s
        return $ case step' of
            Done       -> Done
            Skip    s' -> Skip    (z    , s')
            Yield x s' -> Yield z (f z x, s')
{-# INLINE [0] scanl #-}

iterate :: Monad m => (a -> a) -> a -> Stream m a
iterate f x0 = Stream next (return x0)
  where
    {-# INLINE next #-}
    next x = return $ Yield x (f x)
{-# INLINE [0] iterate #-}

repeat :: Monad m => a -> Stream m a
repeat x = Stream next (return ())
  where
    {-# INLINE next #-}
    next _ = return $ Yield x ()
{-# INLINE [0] repeat #-}
{-# RULES
"map/repeat" forall f x.
    map f (repeat x) = repeat (f x)
  #-}

replicate :: Monad m => Int -> a -> Stream m a
replicate n x = Stream next (return n)
  where
    {-# INLINE next #-}
    next !i | i <= 0    = return Done
            | otherwise = return $ Yield x (i-1)
{-# INLINE [0] replicate #-}
{-# RULES
"map/replicate" forall f n x.
    map f (replicate n x) = replicate n (f x)
  #-}

-- | Unlike 'Data.List.cycle', this function does not diverge if the 'Stream' is
-- empty. Instead, it is the identity in this case.
cycle :: (Monad m) => Stream m a -> Stream m a
cycle (Stream next0 s0) = Stream next ((,) S1 <$> s0)
  where
    {-# INLINE next #-}
    next (S1, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done -- error?
            Skip    s' -> Skip    (S1, s')
            Yield x s' -> Yield x (S2, s')

    next (S2, s) = do
        step <- next0 s
        case step of
            Done       -> Skip . (,) S2 <$> s0
            Skip    s' -> return $ Skip    (S2, s')
            Yield x s' -> return $ Yield x (S2, s')
{-# INLINE [0] cycle #-}

unfoldr :: Monad m => (b -> Maybe (a, b)) -> b -> Stream m a
unfoldr f s0 = Stream next (return s0)
  where
    {-# INLINE next #-}
    next s = return $ case f s of
        Nothing      -> Done
        Just (w, s') -> Yield w s'
{-# INLINE [0] unfoldr #-}

-- | Build a stream from a monadic seed (or state function).
unfoldrM :: (Monad m) => (b -> Maybe (a, m b)) -> m b -> Stream m a
unfoldrM f = Stream next
  where
    {-# INLINE next #-}
    next s = case f s of
        Nothing      -> return Done
        Just (w, s') -> Yield w <$> s'
{-# INLINE [0] unfoldrM #-}

isPrefixOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isPrefixOf (Stream nexta sa0) (Stream nextb sb0) = do
    sa0' <- sa0
    sb0' <- sb0
    loop sa0' sb0' Nothing
  where
    loop !sa !sb Nothing = do
        stepa <- nexta sa
        case stepa of
            Done        -> return True
            Skip    sa' -> loop sa' sb Nothing
            Yield x sa' -> loop sa' sb (Just x)

    loop !sa !sb (Just x) = do
        stepb <- nextb sb
        case stepb of
            Done                    -> return False
            Skip    sb'             -> loop sa sb' (Just x)
            Yield y sb' | x == y    -> loop sa sb' Nothing
                        | otherwise -> return False
{-# INLINE [0] isPrefixOf #-}

-- | Note that this is:
--
-- > isSuffixOf a b = reverse a `isPrefixOf` reverse b
--
-- It might be more efficient to construct the 'Stream's in reverse order and
-- use 'isPrefixOf' directly, as 'reverse' is /O(n)/ and requires a finite
-- stream argument.
isSuffixOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isSuffixOf sa sb = do
    ra <- reverse sa
    rb <- reverse sb
    ra `isPrefixOf` rb

take :: (Monad m) => Int -> Stream m a -> Stream m a
take n0 (Stream next0 s0) = Stream next ((,) n0 <$> s0)
  where
    {-# INLINE next #-}
    next (!n, s)
      | n <= 0    = return Done
      | otherwise = do
          step <- next0 s
          return $ case step of
              Done       -> Done
              Skip    s' -> Skip    (n  , s')
              Yield x s' -> Yield x (n-1, s')
{-# INLINE [0] take #-}

drop :: (Monad m) => Int -> Stream m a -> Stream m a
drop n0 (Stream next0 s0) = Stream next ((,) (Just (max 0 n0)) <$> s0)
  where
    {-# INLINE next #-}
    next (Just !n, s)
      | n == 0    = return $ Skip (Nothing, s)
      | otherwise = do
          step <- next0 s
          return $ case step of
              Done       -> Done
              Skip    s' -> Skip (Just  n   , s')
              Yield _ s' -> Skip (Just (n-1), s')
    next (Nothing, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip    (Nothing, s')
            Yield x s' -> Yield x (Nothing, s')
{-# INLINE [0] drop #-}

-- |
--
-- > splitAt n s = (take n s, drop n s)
--
-- Note that the resulting 'Streams' share their state, so do not interleave
-- traversals.
splitAt :: (Monad m) => Int -> Stream m a -> (Stream m a, Stream m a)
-- not the most efficient solution, but allows the stream argument to be
-- infinite
splitAt n s = (take n s, drop n s)
{-# INLINE splitAt #-}

takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile p (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next !s = do
        step <- next0 s
        return $ case step of
            Done                   -> Done
            Skip    s'             -> Skip    s'
            Yield x s' | p x       -> Yield x s'
                       | otherwise -> Done
{-# INLINE [0] takeWhile #-}

dropWhile :: (Monad m) => (a -> Bool) -> Stream m a -> Stream m a
dropWhile p (Stream next0 s0) = Stream next ((,) S1 <$> s0)
  where
    {-# INLINE next #-}
    next (S1, s) = do
        step <- next0 s
        return $ case step of
            Done                   -> Done
            Skip    s'             -> Skip    (S1, s')
            Yield x s' | p x       -> Skip    (S1, s')
                       | otherwise -> Yield x (S2, s')
    next (S2, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip    (S2, s')
            Yield x s' -> Yield x (S2, s')
{-# INLINE [0] dropWhile #-}

span :: (Monad m) => (a -> Bool) -> Stream m a -> (Stream m a, Stream m a)
span p s = (takeWhile p s, dropWhile p s)
{-# INLINE span #-}

break :: (Monad m) => (a -> Bool) -> Stream m a -> (Stream m a, Stream m a)
break p = span (not . p)
{-# INLINE break #-}

zip :: Monad m
    => Stream m a
    -> Stream m b
    -> Stream m (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

zip3 :: Monad m
     => Stream m a
     -> Stream m b
     -> Stream m c
     -> Stream m (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

zip4 :: Monad m
     => Stream m a
     -> Stream m b
     -> Stream m c
     -> Stream m d
     -> Stream m (a, b, c, d)
zip4 = zipWith4 (,,,)
{-# INLINE zip4 #-}

zipWith :: Monad m
        => (a -> b -> c)
        -> Stream m a
        -> Stream m b
        -> Stream m c
zipWith f (Stream nexta sa0) (Stream nextb sb0) =
    Stream next ((,,) Nothing <$> sa0 <*> sb0)
  where
    {-# INLINE next #-}
    next (Nothing, sa, sb) = do
        step <- nexta sa
        return $ case step of
            Done        -> Done
            Skip    sa' -> Skip (Nothing, sa', sb)
            Yield a sa' -> Skip (Just a , sa', sb)

    next (Just a, sa', sb) = do
        step <- nextb sb
        return $ case step of
            Done        -> Done
            Skip    sb' -> Skip          (Just a, sa', sb')
            Yield b sb' -> Yield (f a b) (Nothing, sa', sb')
{-# INLINE [0] zipWith #-}

zipWith3 :: Monad m
         => (a -> b -> c -> d)
         -> Stream m a
         -> Stream m b
         -> Stream m c
         -> Stream m d
zipWith3 f (Stream nexta sa0)
           (Stream nextb sb0)
           (Stream nextc sc0)
    = Stream next ((,,,) Nothing <$> sa0 <*> sb0 <*> sc0)
  where
    {-# INLINE next #-}
    next (Nothing, sa, sb, sc) = do
        step <- nexta sa
        return $ case step of
            Done        -> Done
            Skip    sa' -> Skip (Nothing          , sa', sb, sc)
            Yield a sa' -> Skip (Just (a, Nothing), sa', sb, sc)

    next (Just (a, Nothing), sa', sb, sc) = do
        step <- nextb sb
        return $ case step of
            Done        -> Done
            Skip    sb' -> Skip (Just (a, Nothing), sa', sb', sc)
            Yield b sb' -> Skip (Just (a, Just b ), sa', sb', sc)

    next (Just (a, Just b), sa', sb', sc) = do
        step <- nextc sc
        return $ case step of
            Done        -> Done
            Skip    sc' -> Skip            (Just (a, Just b), sa', sb', sc')
            Yield c sc' -> Yield (f a b c) (Nothing         , sa', sb', sc')
{-# INLINE [0] zipWith3 #-}

zipWith4 :: Monad m
         => (a -> b -> c -> d -> e)
         -> Stream m a
         -> Stream m b
         -> Stream m c
         -> Stream m d
         -> Stream m e
zipWith4 f (Stream nexta sa0)
           (Stream nextb sb0)
           (Stream nextc sc0)
           (Stream nextd sd0)
    = Stream next ((,,,,) Nothing <$> sa0 <*> sb0 <*> sc0 <*> sd0)
  where
    {-# INLINE next #-}
    next (Nothing, sa, sb, sc, sd) = do
        step <- nexta sa
        return $ case step of
            Done        -> Done
            Skip    sa' -> Skip (Nothing          , sa', sb, sc, sd)
            Yield a sa' -> Skip (Just (a, Nothing), sa', sb, sc, sd)

    next (Just (a, Nothing), sa', sb, sc, sd) = do
        step <- nextb sb
        return $ case step of
            Done        -> Done
            Skip sb'    -> Skip (Just (a, Nothing)          , sa', sb', sc, sd)
            Yield b sb' -> Skip (Just (a, Just (b, Nothing)), sa', sb', sc, sd)

    next (Just (a, Just (b, Nothing)), sa', sb', sc, sd) = do
        step <- nextc sc
        return $ case step of
            Done        -> Done
            Skip    sc' -> Skip (Just (a, Just (b, Nothing)), sa', sb', sc', sd)
            Yield c sc' -> Skip (Just (a, Just (b, Just c)) , sa', sb', sc', sd)

    next (Just (a, Just (b, Just c)), sa', sb', sc', sd) = do
        step <- nextd sd
        return $ case step of
            Done        -> Done
            Skip    sd' -> Skip              (Just (a, Just (b, Just c)), sa', sb', sc', sd')
            Yield d sd' -> Yield (f a b c d) (Nothing                   , sa', sb', sc', sd')
{-# INLINE [0] zipWith4 #-}

unzip :: (Monad m) => Stream m (a, b) -> m ([a], [b])
unzip = foldr (\ (a,b) ~(as,bs) -> (a:as, b:bs)) ([],[])
{-# INLINE unzip #-}

unzip3 :: (Monad m) => Stream m (a, b, c) -> m ([a], [b], [c])
unzip3 = foldr (\ (a,b,c) ~(as,bs,cs) -> (a:as, b:bs, c:cs)) ([],[],[])
{-# INLINE unzip3 #-}

unzip4 :: (Monad m) => Stream m (a, b, c, d) -> m ([a], [b], [c], [d])
unzip4 = foldr (\ (a,b,c,d) ~(as,bs,cs,ds) -> (a:as, b:bs, c:cs, d:ds)) ([],[],[],[])
{-# INLINE unzip4 #-}

delete :: (Eq a, Monad m) => a -> Stream m a -> Stream m a
delete = deleteBy (==)
{-# INLINE delete #-}

insert :: (Ord a, Monad m) => a -> Stream m a -> Stream m a
insert = insertBy compare
{-# INLINE insert #-}

deleteBy :: (Monad m)
         => (a -> a -> Bool)
         -> a
         -> Stream m a
         -> Stream m a
deleteBy eq a (Stream next0 s0) = Stream next ((,) S1 <$> s0)
  where
    {-# INLINE next #-}
    next (S1, s) = do
        step <- next0 s
        return $ case step of
            Done                   -> Done
            Skip    s'             -> Skip    (S1, s')
            Yield x s' | a `eq` x  -> Skip    (S2, s')
                       | otherwise -> Yield x (S1, s')

    next (S2, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip    (S2, s')
            Yield x s' -> Yield x (S2, s')
{-# INLINE [0] deleteBy #-}

insertBy :: (Monad m)
         => (a -> a -> Ordering)
         -> a
         -> Stream m a
         -> Stream m a
insertBy cmp x (Stream next0 s0) = Stream next ((,,) S2 Nothing <$> s0)
  where
    {-# INLINE next #-}
    next (S2, Nothing, s) = do
        step <- next0 s
        return $ case step of
            Done                       -> Yield x (S1, Nothing, s ) -- a snoc
            Skip    s'                 -> Skip    (S2, Nothing, s')
            Yield y s' | GT == cmp x y -> Yield y (S2, Nothing, s')
                       | otherwise     -> Yield x (S1, Just y , s ) -- insert

    next (S2, Just _, _) = error "Data.Stream.Monadic.insertBy: impossible"

    next (S1, Just y, s) = return $ Yield y (S1, Nothing, s)

    next (S1, Nothing, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip    (S1, Nothing, s')
            Yield y s' -> Yield y (S1, Nothing, s')
{-# INLINE [0] insertBy #-}

-- not sure why this is defined recursively (unlike 'length')
genericLength :: (Num i, Monad m) => Stream m a -> m i
genericLength (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done       -> return 0
            Skip    s' -> loop s'
            Yield _ s' -> (1 +) <$> loop s'
{-# INLINE [0] genericLength #-}

genericTake :: (Integral i, Monad m) => i -> Stream m a -> Stream m a
genericTake n0 (Stream next0 s0) = Stream next ((,) n0 <$> s0)
  where
    {-# INLINE next #-}
    next (0, _)  = return Done
    next (n, s)  = do
        step <- next0 s
        return $ case step of
            Done          -> Done
            Skip    s'    -> Skip    (n  , s')
            Yield x s'
              | n > 0     -> Yield x (n-1, s')
              | otherwise -> error "List.genericTake: negative argument"
{-# INLINE [0] genericTake #-}

genericDrop :: (Integral i, Monad m) => i -> Stream m a -> Stream m a
genericDrop n0 (Stream next0 s0) = Stream next ((,) (Just n0) <$> s0)
  where
    {-# INLINE next #-}
    next (Just 0, s) = return $ Skip (Nothing, s)
    next (Just n, s) = do
        step <- next0 s
        return $ case step of
            Done                    -> Done
            Skip    s'              -> Skip (Just n    , s')
            Yield _ s' | n > 0      -> Skip (Just (n-1), s')
                       | otherwise  -> error "List.genericDrop: negative argument"

    next (Nothing, s) = do
        step <- next0 s
        return $ case step of
            Done       -> Done
            Skip    s' -> Skip    (Nothing, s')
            Yield x s' -> Yield x (Nothing, s')
{-# INLINE [0] genericDrop #-}

genericSplitAt :: (Integral i, Monad m)
               => i
               -> Stream m a
               -> (Stream m a, Stream m a)
genericSplitAt i s = (genericTake i s, genericDrop i s)
{-# INLINE genericSplitAt #-}

genericReplicate :: (Integral i, Monad m) => i -> a -> Stream m a
genericReplicate n = genericTake n . repeat
{-# INLINE [0] genericReplicate #-}
{-# RULES
"genericReplicate -> replicate/Int"
    genericReplicate = replicate :: Monad m => Int -> a -> Stream m a
  #-}

-- TODO: is it possible to define rules which would rewrite @fromList [n..m]@ to
-- one of the below?

-- | Like @fromList ([n..m] :: [Int])@ but avoids allocating a list
enumFromToInt :: Monad m => Int -> Int -> Stream m Int
enumFromToInt x y = Stream next (return x)
  where
    {-# INLINE next #-}
    next !n
      | n > y     = return Done
      | otherwise = return $ Yield n (n+1)
{-# INLINE [0] enumFromToInt #-}

-- | Like @fromList ([n,n+d..] :: [Integer])@ but avoids allocating a list
enumDeltaInteger :: Monad m => Integer -> Integer -> Stream m Integer
enumDeltaInteger a d = Stream next (return a)
  where
    {-# INLINE next #-}
    next !x = return $ Yield x (x+d)
{-# INLINE [0] enumDeltaInteger #-}

-- | Like @fromList ([n..m] :: [Char])@ but avoids allocating a list
enumFromToChar :: Monad m => Char -> Char -> Stream m Char
enumFromToChar x y = Stream next (return (ord x))
  where
    m = ord y

    {-# INLINE next #-}
    next !n
      | n > m     = return Done
      | otherwise = return $ Yield (chr n) (n+1)
{-# INLINE [0] enumFromToChar #-}
