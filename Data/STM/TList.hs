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
module Data.STM.TList where

import Prelude hiding (drop, length)
import qualified Prelude

import Control.Concurrent.STM hiding (check)
import Control.Exception (Exception)
import Control.Monad (foldM, join)
import Data.Typeable (Typeable)

type TList a = TVar (TCell a)
data TCell a = TNil | TCons a (TList a)
    deriving Typeable

-- | Append an item to the list, returning the new write end.
--
-- The 'TList' normally points to a 'TNil', a \"hole\" into which the next item
-- will be written.  However, if it doesn't, 'append' will silently overwrite
-- the next item.  It is up to the application to ensure that the 'TList'
-- points to a 'TNil', or that overwriting an item in this case is desirable.
append :: TList a -> a -> STM (TList a)
append hole x = do
    hole' <- newTVar TNil
    writeTVar hole (TCons x hole')
    return hole'

-- | Append a list of items, returning the new write end.
appendList :: TList a -> [a] -> STM (TList a)
appendList = foldM append

-- | Get the next item of the list (if available).  Handle 'TNil' (no items
-- available) or 'TCons' (next item) using the appropriate continuation.
uncons :: TList a
       -> STM b
            -- ^ What to do if the list is empty
       -> (a -> TList a -> STM b)
            -- ^ What to do with the item and the remainder of the list
       -> STM b
uncons tl onNil onCons = do
    cell <- readTVar tl
    case cell of
        TNil       -> onNil
        TCons x xs -> onCons x xs
{-# INLINE uncons #-}

-- | Skip the given number of items.  Return the end of the list if a 'TNil' is
-- reached.
drop :: Int -> TList a -> STM (TList a)
drop n xs
    | n <= 0    = return xs
    | otherwise = uncons xs (return xs) (\_ xs' -> drop (n-1) xs')

-- | Traverse the list, stopping when a 'TNil' is reached.
end :: TList a -> STM (TList a)
end xs = uncons xs (return xs) (\_ xs' -> end xs')

-- | Traverse the list, returning its length.
length :: TList a -> STM Int
length list = len list 0
    where
        len xs !n = uncons xs (return n) (\_ xs' -> len xs' (n+1))

-- | Traverse the list with an accumulator function and initial value.
foldl' :: (a -> b -> a) -> a -> TList b -> STM a
foldl' f a xs =
    uncons xs
           (return a)
           (\x xs' -> let !a' = f a x
                       in foldl' f a' xs')

-- | Traverse a 'TList', returning its items as a pure list.
toList :: TList a -> STM [a]
toList list = loop id list
    where
        loop !dl xs =
            uncons xs
                   (return $ dl [])
                   (\x xs' -> loop (dl . (x:)) xs')

-- | Convert a pure list to a 'TList', returning the head (read end) and tail
-- (write end) of the list.
fromList :: [a] -> STM (TList a, TList a)
fromList xs = do
    readEnd <- newTVar TNil
    writeEnd <- appendList readEnd xs
    return (readEnd, writeEnd)
