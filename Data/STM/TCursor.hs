-- |
-- Module:       Data.STM.TCursor
-- Copyright:    (c) Joseph Adams 2012
-- License:      BSD3
-- Maintainer:   joeyadams3.14159@gmail.com
-- Portability:  Requires STM
--
-- This module provides an API very similar to "Control.Concurrent.STM.TChan".
-- However, unlike 'TChan':
--
--  * It is based on "Data.STM.TList", rather than using an abstract internal
--    representation.
--
--  * It separates the read end and write end.  This means if the channel has no
--    readers, items written with 'writeTCursor' can be garbage collected.
--
-- Here is an implementation of 'TChan' based on 'TCursor':
--
-- >type TChan a = (TCursor a, TCursor a)
-- >
-- >newTChan = newTCursorPair
-- >
-- >newTChanIO = newTCursorPairIO
-- >
-- >readTChan = readTCursor . fst
-- >
-- >writeTChan = writeTCursor . snd
-- >
-- >dupTChan (_, writeEnd) = do
-- >    newReadEnd <- dupTCursor writeEnd
-- >    return (newReadEnd, writeEnd)
-- >
-- >unGetTChan = unGetTCursor . fst
-- >
-- >isEmptyTChan = isEmptyTCursor . fst
module Data.STM.TCursor (
    -- * The TCursor type
    -- $tcursor
    TCursor,

    -- * Construction
    newTCursorPair,
    newTCursorPairIO,
    dupTCursor,

    -- * Reading and writing
    readTCursor,
    tryReadTCursor,
    writeTCursor,
    unGetTCursor,
    isEmptyTCursor,
) where

import Prelude hiding (null)

import Data.STM.TList

import Control.Concurrent.STM hiding (check)
import Control.Monad (liftM2)

-- | A 'TCursor' is a mutable cursor used for traversing items.  While 'uncons'
-- and 'append' return the subsequent 'TList', 'readTCursor' and 'writeTCursor'
-- modify the cursor in-place, and thus behave more like 'readTChan' and
-- 'writeTChan'.
type TCursor a = TVar (TList a)

-- | /O(1)/.  Construct an empty channel, returning the read cursor ('fst') and
-- write cursor ('snd').
newTCursorPair :: STM (TCursor a, TCursor a)
newTCursorPair = do
    hole <- empty
    liftM2 (,) (newTVar hole) (newTVar hole)

-- | /O(1)/.  'IO' variant of 'newCursorPair'.  See 'newTVarIO' for the
-- rationale.
newTCursorPairIO :: IO (TCursor a, TCursor a)
newTCursorPairIO = do
    hole <- emptyIO
    liftM2 (,) (newTVarIO hole) (newTVarIO hole)

-- | /O(1)/.  Read the next item and advance the cursor.  'retry' if the
-- channel is currently empty.
--
-- This should be called on the /read/ cursor of the channel.
readTCursor :: TCursor a -> STM a
readTCursor cursor =
    readTVar cursor >>=
        uncons retry
               (\x xs -> do writeTVar cursor xs
                            return x)

-- | /O(1)/.  Like 'readTCursor', but return 'Nothing', rather than 'retry'ing,
-- if the list is currently empty.
tryReadTCursor :: TCursor a -> STM (Maybe a)
tryReadTCursor cursor =
    readTVar cursor >>=
        uncons (return Nothing)
               (\x xs -> do writeTVar cursor xs
                            return (Just x))

-- | /O(1)/.  Append an item and advance the cursor.
--
-- This should be called on the /write/ cursor of the channel.  See 'append'
-- for more details.
writeTCursor :: TCursor a -> a -> STM ()
writeTCursor cursor x =
    readTVar cursor >>= flip append x >>= writeTVar cursor

-- | /O(1)/.  Make a copy of a 'TCursor'.  Modifying the old cursor with
-- 'readTCursor' or 'writeTCursor' will not affect the new cursor, and vice
-- versa.
dupTCursor :: TCursor a -> STM (TCursor a)
dupTCursor cursor = readTVar cursor >>= newTVar

-- | /O(1)/.  Put an item back on the channel, where it will be the next item
-- read by 'readTCursor'.
--
-- This should be called on the /read/ cursor of the channel.
unGetTCursor :: TCursor a -> a -> STM ()
unGetTCursor cursor x =
    readTVar cursor >>= cons x >>= writeTVar cursor

-- | /O(1)/.  Return 'True' if the channel is empty.
--
-- This should be called on the /read/ cursor of the channel.
isEmptyTCursor :: TCursor a -> STM Bool
isEmptyTCursor cursor = readTVar cursor >>= null
