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
import Control.Monad (join)
import Data.Typeable (Typeable)

type TList a = TVar (TCell a)
data TCell a = TNil | TCons a (TList a)
    deriving Typeable

-- | Get the next item of the list (if available), and handle it with the given
-- continuation.
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
