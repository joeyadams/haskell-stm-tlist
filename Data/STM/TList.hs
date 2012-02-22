-- |
-- Module:       Data.STM.TList
-- Copyright:    (c) Joseph Adams 2012
-- License:      BSD3
-- Maintainer:   joeyadams3.14159@gmail.com
-- Portability:  Requires STM
--
-- This module uses many names from Prelude, so consider importing it
-- qualified:
--
-- >import Data.STM.TList (TList)
-- >import qualified Data.STM.TList as TList
{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module Data.STM.TList (
    -- * The TList type
    TList,
    TCell(..),

    -- * Construction
    empty,
    emptyIO,
    cons,
    append,
    appendList,
    fromList,

    -- * Traversal
    -- | These functions traverse the list strictly.  They examine the list as
    -- it is now; they do not 'retry' when the end of the list is reached.
    uncons,
    null,
    drop,
    end,
    length,
    foldl',
    toList,

    -- * Working with mutable cursors
    TCursor,
    newCursorPair,
    newCursorPairIO,
    read,
    tryRead,
    write,
) where

import Prelude hiding (drop, length, null, read)

import Control.Concurrent.STM hiding (check)
import Control.Monad (liftM2, foldM)
import Data.Typeable (Typeable)

------------------------------------------------------------------------
-- The TList type

-- | A 'TList' is a mutable linked list node.  A 'TList' node containing 'TNil'
-- is usually called a \"hole\" or \"write end\", and can be \"filled\" using
-- 'append'.
type TList a = TVar (TCell a)

data TCell a = TNil | TCons a !(TList a)
    deriving Typeable

------------------------------------------------------------------------
-- Construction

-- | /O(1)/.  Construct a new, empty 'TList'.
empty :: STM (TList a)
empty = newTVar TNil

-- | /O(1)/.  'IO' variant of 'empty'.  See 'newTVarIO' for the rationale.
emptyIO :: IO (TList a)
emptyIO = newTVarIO TNil

-- | /O(1)/.  Prepend an item to the list, returning the new beginning of the
-- list.
cons :: a -> TList a -> STM (TList a)
cons x xs = newTVar (TCons x xs)

-- | /O(1)/.  Append an item to the list, returning the new write end.
--
-- The 'TList' normally points to a 'TNil', a \"hole\" into which the next item
-- will be written.  However, if it doesn't, 'append' will silently overwrite
-- the next item.  It is up to the application to ensure that the 'TList'
-- points to a 'TNil', or that overwriting an item in this case is desirable.
append :: TList a -> a -> STM (TList a)
append hole x = do
    hole' <- empty
    writeTVar hole (TCons x hole')
    return hole'

-- | /O(n)/.  Append a list of items, returning the new write end.
appendList :: TList a -> [a] -> STM (TList a)
appendList = foldM append

-- | /O(n)/.  Convert a pure list to a 'TList', returning the head (read end)
-- and tail (write end) of the list.
fromList :: [a] -> STM (TList a, TList a)
fromList xs = do
    readEnd <- empty
    writeEnd <- appendList readEnd xs
    return (readEnd, writeEnd)

------------------------------------------------------------------------
-- Traversal

-- | /O(1)/.  Get the next item of the list (if available).  Handle 'TNil' (no
-- items available) or 'TCons' (next item) using the appropriate continuation.
--
-- The 'TList' argument being at the end means 'uncons' can be partially
-- applied in many situations.
uncons :: STM b
            -- ^ What to do if the list is empty
       -> (a -> TList a -> STM b)
            -- ^ What to do with the item and the remainder of the list
       -> TList a
            -- ^ List node to examine
       -> STM b
uncons onNil onCons tl = do
    cell <- readTVar tl
    case cell of
        TNil       -> onNil
        TCons x xs -> onCons x xs
{-# INLINE uncons #-}

-- | /O(1)/.  Return 'True' if the list is empty.
null :: TList a -> STM Bool
null = uncons (return True) (\_ _ -> return False)

-- | /O(n)/.  Skip the given number of items.  Return the end of the list if a
-- 'TNil' is reached.
drop :: Int -> TList a -> STM (TList a)
drop n xs
    | n <= 0    = return xs
    | otherwise = uncons (return xs) (\_ xs' -> drop (n-1) xs') xs

-- | /O(n)/.  Traverse the list, stopping when a 'TNil' is reached.
--
-- Bear in mind that 'TList's are mutable.  In particular, the 'end' of a
-- 'TList' is not as boring as the end of a pure list (@[]@, a.k.a.
-- \"nil\").  It is usually the write end, to which additional items may be
-- 'append'ed.
end :: TList a -> STM (TList a)
end xs = uncons (return xs) (\_ xs' -> end xs') xs

-- | /O(n)/.  Traverse the list, returning its length.
length :: TList a -> STM Int
length = len 0
    where
        len !n = uncons (return n) (\_ -> len (n+1))

-- | /O(n)/.  Traverse the list with an accumulator function and initial value.
foldl' :: (a -> b -> a) -> a -> TList b -> STM a
foldl' f a =
    uncons (return a)
           (\x -> let !a' = f a x
                   in foldl' f a')

-- | /O(n)/.  Traverse a 'TList', returning its items as a pure list.
toList :: TList a -> STM [a]
toList = loop id
    where
        loop !dl =
            uncons (return $ dl [])
                   (loop . (dl .) . (:))

------------------------------------------------------------------------
-- Working with mutable cursors

-- | A 'TCursor' is a mutable cursor used for traversing items.  While 'uncons'
-- and 'append' return the subsequent 'TList', 'read' and 'write' modify the
-- 'TCursor' in-place, and thus behave more like 'readTChan' and 'writeTChan'.
type TCursor a = TVar (TList a)

-- | /O(1)/.  Construct an empty channel, returning the read cursor ('fst') and
-- write cursor ('snd').
newCursorPair :: STM (TCursor a, TCursor a)
newCursorPair = do
    hole <- empty
    liftM2 (,) (newTVar hole) (newTVar hole)

-- | /O(1)/.  'IO' variant of 'newCursorPair'.  See 'newTVarIO' for the
-- rationale.
newCursorPairIO :: IO (TCursor a, TCursor a)
newCursorPairIO = do
    hole <- emptyIO
    liftM2 (,) (newTVarIO hole) (newTVarIO hole)

-- | /O(1)/.  Read the next item and advance the cursor.  'retry' if the list
-- is currently empty.
--
-- This should be called on the read cursor of the channel.
read :: TCursor a -> STM a
read cursor =
    readTVar cursor >>=
        uncons retry
               (\x xs -> do writeTVar cursor xs
                            return x)

-- | /O(1)/.  Like 'read', but return 'Nothing', rather than 'retry'ing, if the
-- list is currently empty.
tryRead :: TCursor a -> STM (Maybe a)
tryRead cursor =
    readTVar cursor >>=
        uncons (return Nothing)
               (\x xs -> do writeTVar cursor xs
                            return (Just x))

-- | /O(1)/.  Append an item and advance the cursor.  This should be called on
-- the write cursor of the channel.  See 'append' for more details.
write :: TCursor a -> a -> STM ()
write cursor x =
    readTVar cursor >>= flip append x >>= writeTVar cursor
