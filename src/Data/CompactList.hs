-- |
-- Module      : Data.CompactList
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) 2001-14 GHC Project
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- If you hold on to a large data structure in garbage collected (GC) memory
-- for relatively longer times it puts undue pressure on GC, unnecessarily
-- increasing the work done by GC and also increasing the duration of GC
-- pauses. A 'CompactList' allows you to keep a large list in a Compact Region
-- not touched by GC, thus avoiding any GC overhead.  This is essentially like
-- a convenient in-memory append only file where you can write a list of
-- Haskell values without having to marshall or serialize them.
--
-- A 'CompactList' is like a thread safe 'IORef' to a list in compact region.

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.CompactList
    ( CompactList
    , newCompactList
    , consCompactList
    , readCompactList
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import GHC.IO (stToIO)
import GHC.Prim
       (Compact#, compactAdd#, compactNew#, int2Word#, RealWorld, State#)
import GHC.ST (ST(ST))
import GHC.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import GHC.Types (Int(I#), IO(IO))

-- | A list with values of type @a@ living in a compact (non-GC) region.
data CompactList a = CompactList Compact# (STRef RealWorld [a]) (MVar ())

mkCompactList
  :: Compact# -> [a] -> State# RealWorld -> (# State# RealWorld, CompactList a #)
mkCompactList compact# as s =
  case unST (newSTRef as) s of { (# s0, ref #) ->
      case unIO (newMVar ()) s0 of { (# s1, lock #) ->
      (# s1, CompactList compact# ref lock #) } }
 where
  unST (ST a) = a
  unIO (IO a) = a

compactSized :: Int -> [a] -> IO (CompactList a)
compactSized (I# size) a = IO $ \s0 ->
  case compactNew# (int2Word# size) s0 of { (# s1, compact# #) ->
  case compactAdd# compact# a s1 of { (# s2, pk #) ->
  mkCompactList compact# pk s2 }}

-- | Make a new compact list from a list.
newCompactList :: [a] -> IO (CompactList a)
newCompactList = compactSized 31268

-- | Retrieve the compact list from the compact region.
readCompactList :: CompactList a -> IO [a]
readCompactList (CompactList _ ref _) = stToIO (readSTRef ref)

-- | Add an element at the head of the compact list. The modification is thread
-- safe.
consCompactList :: CompactList a -> a -> IO ()
consCompactList (CompactList compact# ref lock) x =
    withMVar lock $ \_ -> IO $ \s ->
        case unST (readSTRef ref) s of { (# s1, xs #) ->
            case compactAdd# compact# (x : xs) s1 of { (# s2, ys #) ->
              unST (writeSTRef ref ys) s2 }}
     where
      unST (ST a) = a
