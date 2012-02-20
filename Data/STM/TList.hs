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
-- >import Data.STM.TList (TList(..))
-- >import qualified Data.STM.TList as TList
{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
module Data.STM.TList (
) where

import Prelude hiding (length, read)
import qualified Prelude

import Control.Concurrent.STM hiding (check)
import Control.Exception (Exception)
import Control.Monad (join)
import Data.Typeable (Typeable)

type TCell a = TVar (TList a)
data TList a = TNil | TCons a (TCell a)

-- | TCell variant of 'drop'.  This does not modify the cells themselves, it
-- just returns the new pointer.
dropItems :: Int -> TCell a -> STM (TCell a)
dropItems n cell
    | n <= 0    = return cell
    | otherwise = do
        xs <- readTVar cell
        case xs of
            TNil          -> return cell
            TCons _ cell' -> dropItems (n-1) cell'

traverseLength :: TCell a -> STM Int
traverseLength = loop 0
    where
        loop !n cell = do
            xs <- readTVar cell
            case xs of
                TNil          -> return n
                TCons _ cell' -> loop (n+1) cell'

toList :: TCell a -> STM [a]
toList list = loop id list
    where
        loop dl cell = do
            xs <- readTVar cell
            case xs of
                TNil          -> return $ dl []
                TCons x cell' -> loop (dl . (x :)) cell'
