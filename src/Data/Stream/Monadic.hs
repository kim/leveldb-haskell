{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}

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
    , length

    -- * Transformations
    , map
    -- , mapM
    , intersperse

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
    -- , concat
    , concatMap
    -- , and
    -- , or
    -- , any
    -- , all
    -- , sum
    -- , product
    -- , maximum
    -- , minimum

    -- , scanl
    -- , scanl1

    -- * Infinite streams
    , iterate
    , repeat
    , replicate
    , cycle

    -- * Unfolding
    , unfoldr
    , unfoldrM

    -- , isPrefixOf

    -- * Searching streams
    -- , elem
    -- , lookup

    -- , find
    , filter

    -- , index
    -- , findIndex
    -- , elemIndex
    -- , elemIndices
    -- , findIndices

    -- * Substreams
    , take
    , drop
    -- , splitAt
    , takeWhile
    , dropWhile

    -- * Zipping and unzipping
    , zip
    -- , zip3
    -- , zip4
    , zipWith
    -- , zipWith3
    -- , zipWith4
    , unzip

    -- , insertBy
    -- , maximumBy
    -- , minimumBy

    -- , genericLength
    -- , genericTake
    -- , genericDrop
    -- , genericIndex
    -- , genericSplitAt

    -- , enumFromToInt
    -- , enumFromToChar
    -- , enumDeltaInteger
    )
where

import Control.Applicative
import Data.Monoid

import Prelude (Bool (..), Either (..), Eq (..), Functor (..), Int, Maybe (..),
                Monad (..), Num (..), Ord (..), otherwise, ($), (&&), (=<<), (.))


data Step   a  s
   = Yield  a !s
   | Skip  !s
   | Done

data Stream m a = forall s. Stream (s -> m (Step a s)) (m s)

instance Monad m => Functor (Stream m) where
    fmap = map


toList :: (Functor m, Monad m) => Stream m a -> m [a]
toList (Stream next s0) = unfold =<< s0
  where
    unfold !s = do
        step <- next s
        case step of
            Done       -> return []
            Skip    s' -> unfold s'
            Yield x s' -> (x :) <$> unfold s'

fromList :: Monad m => [a] -> Stream m a
fromList xs = Stream next (return xs)
  where
    {-# INLINE next #-}
    next []      = return Done
    next (x:xs') = return $ Yield x xs'

{-# RULES
    "Stream fromList/toList fusion" forall s.
        fmap fromList (toList s) = return s
  #-}

append :: (Functor m, Monad m) => Stream m a -> Stream m a -> Stream m a
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

cons :: (Functor m, Monad m) => a -> Stream m a -> Stream m a
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

snoc :: (Functor m, Monad m) => Stream m a -> a -> Stream m a
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

tail :: (Functor m, Monad m) => Stream m a -> Stream m a
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

init :: (Functor m, Monad m) => Stream m a -> Stream m a
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
        filter p (filter q s) = filter (\x -> q x && p x) s
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
        map f (map g s) = map (\x -> f (g x)) s
  #-}

-- 'mapM' is tricky:
--
-- > mapM :: (Monad m, Monad n) => (a -> n b) -> Stream m a -> n (Stream n b)
--
-- we would need a constraint which specifies how to lift any monad /m/ into
-- some monad /n/ (or specialise /m/ to 'IO').
--
-- alternatively, we may define:
--
-- > mapM :: Monad m => (a -> m b) -> Stream m a -> m (Stream m b)
--
-- or rather:
--
-- > mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
--
-- not sure how useful this would be.


intersperse :: (Functor m, Monad m) => a -> Stream m a -> Stream m a
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
{-# INLINE [0] intersperse #-}

foldMap :: (Monoid m, Functor n, Monad n) => (a -> m) -> Stream n a -> n m
foldMap f (Stream next s0) = loop mempty =<< s0
  where
    loop z !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop z s'
            Yield x s' -> loop (z <> f x) s'
{-# INLINE [0] foldMap #-}

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

foldr :: (Functor m, Monad m) => (a -> b -> b) -> b -> Stream m a -> m b
foldr f z (Stream next s0) = loop =<< s0
  where
    loop !s = do
        step <- next s
        case step of
            Done       -> return z
            Skip    s' -> loop s'
            Yield x s' -> f x <$> loop s'
{-# INLINE [0] foldr #-}

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

foldM_ :: Monad m => (b -> a -> m b) -> b -> Stream m a -> m ()
foldM_ f z0 (Stream next s0) = loop z0 =<< s0
  where
    loop z !s = do
        step <- next s
        case step of
            Done       -> return ()
            Skip    s' -> loop z s'
            Yield x s' -> f z x >>= (`loop` s')
{-# INLINE [0] foldM_ #-}

concatMap :: (Functor m, Monad m) => (a -> Stream m b) -> Stream m a -> Stream m b
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
    "map/repeat" forall f x. map f (repeat x) = repeat (f x)
  #-}

replicate :: Monad m => Int -> a -> Stream m a
replicate n x = Stream next (return n)
  where
    {-# INLINE next #-}
    next !i | i <= 0    = return Done
            | otherwise = return $ Yield x (i-1)
{-# INLINE [0] replicate #-}
{-# RULES
    "map/replicate" forall f n x. map f (replicate n x) = replicate n (f x)
  #-}

cycle :: (Functor m, Monad m) => Stream m a -> Stream m a
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
            Done       -> Skip . ((,) S2) <$> s0
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

unfoldrM :: (Functor m, Monad m) => (b -> Maybe (a, m b)) -> m b -> Stream m a
unfoldrM f s0 = Stream next s0
  where
    {-# INLINE next #-}
    next s = case f s of
        Nothing      -> return Done
        Just (w, s') -> Yield w <$> s'
{-# INLINE [0] unfoldrM #-}

take :: (Functor m, Monad m) => Int -> Stream m a -> Stream m a
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

drop :: (Functor m, Monad m) => Int -> Stream m a -> Stream m a
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

dropWhile :: (Functor m, Monad m) => (a -> Bool) -> Stream m a -> Stream m a
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

zip :: (Functor m, Applicative m, Monad m)
    => Stream m a
    -> Stream m b
    -> Stream m (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

zipWith :: (Functor m, Applicative m, Monad m)
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

unzip :: (Functor m, Monad m) => Stream m (a, b) -> m ([a], [b])
unzip = foldr (\(a,b) ~(as, bs) -> (a:as, b:bs)) ([], [])
{-# INLINE unzip #-}
