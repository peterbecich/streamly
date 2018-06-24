-- |
-- Module      : BenchmarkOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}

module LinearOps where

import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined, Maybe(..))

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

value, maxValue :: Int
value = 100000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE fmap #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE zip #-}
{-# INLINE concat #-}
{-# INLINE composeAllInFilters #-}
{-# INLINE composeAllOutFilters #-}
{-# INLINE composeMapAllInFilter #-}
scan, map, fmap, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Stream m Int -> m ()

{-# INLINE composeMapM #-}
composeMapM :: S.MonadAsync m => Stream m Int -> m ()

{-# INLINE toList #-}
{-# INLINE foldr #-}
{-# INLINE foldrM #-}
toList, foldr, foldrM :: Monad m => Stream m Int -> m [Int]
{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
{-# INLINE toNull #-}
toNull :: Monad m => (t m Int -> S.SerialT m Int) -> t m Int -> m ()
{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m a -> m ()
{-# INLINE mapM #-}
mapM :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> t m Int -> m ()
{-# INLINE zipAsync #-}
zipAsync :: S.MonadAsync m => Stream m Int -> m ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.SerialT m a

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> t m Int
source n = S.serially $ sourceUnfoldrM n
-- source n = S.serially $ sourceFromList n

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, S.IsStream t) => Int -> t m Int
sourceFromList n = S.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (S.MonadAsync m, S.IsStream t) => Int -> t m Int
sourceFromListM n = S.fromListM (Prelude.fmap return [n..n+value])

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: S.IsStream t => Int -> t m Int
sourceFromFoldable n = S.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: (S.IsStream t, S.MonadAsync m) => Int -> t m Int
sourceFromFoldableM n = S.fromFoldableM (Prelude.fmap return [n..n+value])

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (S.IsStream t, Monad (t m), S.Semigroup (t m Int))
    => Int -> t m Int
sourceFoldMapWith n = S.foldMapWith (S.<>) return [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: (S.IsStream t, Monad m, S.Semigroup (t m Int))
    => Int -> t m Int
sourceFoldMapWithM n = S.foldMapWith (S.<>) (S.yieldM . return) [n..n+value]

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> t m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> t m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull t = runStream . t
mapM_  = S.mapM_ (\_ -> return ())
toList = S.toList
foldr  = S.foldr (:) []
foldrM = S.foldrM (\a xs -> return (a : xs)) []
foldl  = S.foldl' (+) 0
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

scan          = transform . S.scanl' (+) 0
fmap          = transform . Prelude.fmap (+1)
map           = transform . S.map (+1)
mapM t        = transform . t . S.mapM return
filterEven    = transform . S.filter even
filterAllOut  = transform . S.filter (> maxValue)
filterAllIn   = transform . S.filter (<= maxValue)
takeOne       = transform . S.take 1
takeAll       = transform . S.take maxValue
takeWhileTrue = transform . S.takeWhile (<= maxValue)
dropAll       = transform . S.drop maxValue
dropWhileTrue = transform . S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip src       = transform $ (S.zipWith (,) src src)
zipAsync src  = transform $ (S.zipAsyncWith (,) src src)
concat _n     = return ()

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: Monad m => (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
compose f = transform . f . f . f . f

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter =
    compose (S.filter (<= maxValue) . Prelude.fmap (subtract 1))

{-# INLINABLE composeScaling #-}
composeScaling :: Monad m => Int -> Stream m Int -> m ()
composeScaling m =
    case m of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined
    where f = S.filter (<= maxValue)
