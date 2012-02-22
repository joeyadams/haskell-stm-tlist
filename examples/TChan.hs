-- | A concise, perhaps even cryptic, re-implementation of
-- 'Control.Concurrent.STM.TChan' using 'TList'.
{-# LANGUAGE RecordWildCards #-}
module TChan (
    TChan,
    newTChan,
    newTChanIO,

    readTChan,
    writeTChan,
    dupTChan,
    unGetTChan,
    isEmptyTChan,
) where

import Control.Concurrent.STM
    (STM, TVar, newTVar, newTVarIO, readTVar, writeTVar, retry)
import Control.Monad (liftM2)
import Data.STM.TList (TList)
import qualified Data.STM.TList as TList

data TChan a
    = TChan
        { readEnd  :: TVar (TList a)
        , writeEnd :: TVar (TList a)
        }

newTChan :: STM (TChan a)
newTChan = do
    hole <- TList.empty
    liftM2 TChan (newTVar hole) (newTVar hole)

newTChanIO :: IO (TChan a)
newTChanIO = do
    hole <- TList.emptyIO
    liftM2 TChan (newTVarIO hole) (newTVarIO hole)

readTChan :: TChan a -> STM a
readTChan TChan{..} =
    readTVar readEnd >>=
        TList.uncons retry
                     (\x xs -> writeTVar readEnd xs >> return x)

writeTChan :: TChan a -> a -> STM ()
writeTChan TChan{..} x =
    readTVar writeEnd >>= flip TList.append x >>= writeTVar writeEnd

dupTChan :: TChan a -> STM (TChan a)
dupTChan TChan{..} = do
    newReadEnd <- newTVar =<< readTVar writeEnd
    return (TChan newReadEnd writeEnd)

unGetTChan :: TChan a -> a -> STM ()
unGetTChan TChan{..} x =
    readTVar readEnd >>= TList.cons x >>= writeTVar readEnd

isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan TChan{..} =
    readTVar readEnd >>=
        TList.uncons (return True)
                     (\_ _ -> return False)
